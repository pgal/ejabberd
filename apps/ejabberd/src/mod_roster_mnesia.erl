-module(mod_roster_mnesia).

-compile([export_all]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").

create_table() ->
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
    				{attributes, record_info(fields, roster_version)}]),

    update_table(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us).

delete_item(LUser, LServer, LJID) ->
	mnesia:delete({roster, {LUser, LServer, LJID}}).

remove_user_storage(US) ->
    F = fun() ->
		lists:foreach(fun(R) ->
				      mnesia:delete_object(R)
			      end,
			      mnesia:index_read(roster, US, #roster.us))
        end,
    mnesia:transaction(F).

read_roster_version_storage(US) ->
	mnesia:dirty_read(roster_version, US).

write_to_storage(Rec, Opts) ->
	case proplists:get_value(dirty, Opts, false) of
		true ->
			mnesia:dirty_write(Rec);
		false ->
			mnesia:write(Rec)
    end.
	
write_to_roster_storage(Rec) ->
    mnesia:write(Rec).
	
write_to_roster_version_storage(Rec) ->
    mnesia:dirty_write(Rec).
	
read_user_roster(US) ->
	mnesia:dirty_index_read(roster, US, #roster.us).

read_roster(User) ->
	mnesia:dirty_read(roster, User).


roster_version_storage(US) ->
	case read_roster_version_storage(US) of
		[#roster_version{version = V}] ->
			V;
		[] ->
			not_found
	end.


set_items_storage(LUser, LServer, Els) ->
    F = fun() ->
		lists:foreach(fun(El) ->
			mod_roster:process_item_set_t(LUser, LServer, El)
			end, Els)
	end,
    mnesia:transaction(F).

process_item_set_storage(Attrs, Els, User, JID, LUser, LServer, LJID) ->
    F = fun() ->
		Res = mnesia:read({roster, {LUser, LServer, LJID}}),
		Item = case Res of
			   [] ->
			       #roster{usj = {LUser, LServer, LJID},
				       us = {LUser, LServer},
				       jid = JID};
			   [I] ->
			       I#roster{jid = JID,
					name = "",
					groups = [],
					xs = []}
		       end,
		Item1 = mod_roster:process_item_attrs(Item, Attrs),
		Item2 = mod_roster:process_item_els(Item1, Els),
		case Item2#roster.subscription of
		    remove ->
				delete_item(User, LServer, LJID);
		    _ ->
				mnesia:write(Item2)
		end,
		%% If the item exist in shared roster, take the
		%% subscription information from there:
		Item3 = ejabberd_hooks:run_fold(roster_process_item,
						LServer, Item2, [LServer]),
		case mod_roster:roster_version_on_db(LServer) of
			true -> 
				mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
			false ->
				ok
		end,
		{Item, Item3}
	end,
    
    case mnesia:transaction(F) of
        {atomic, Result} ->
            {ok, Result};
        Error ->
            {error, Error}
    end.


process_subscription_storage(Direction, Type, Reason, JID1, US, LUser, LServer, LJID) ->
    F = fun() ->
		Item = case mnesia:read({roster, {LUser, LServer, LJID}}) of
			   [] ->
			       JID = {JID1#jid.user,
				      JID1#jid.server,
				      JID1#jid.resource},
			       #roster{usj = {LUser, LServer, LJID},
				       us = US,
				       jid = JID};
			   [I] ->
			       I
		       end,
		NewState = case Direction of
					out ->
						mod_roster:out_state_change(Item#roster.subscription,
						    Item#roster.ask,
						    Type);
					in ->
						mod_roster:in_state_change(Item#roster.subscription,
						   Item#roster.ask,
						   Type)
			   end,
		AutoReply = case Direction of
				out ->
				    none;
				in ->
				    mod_roster:in_auto_reply(Item#roster.subscription,
						  Item#roster.ask,
						  Type)
			    end,
		AskMessage = case NewState of
					{_, both} ->
						Reason;
					{_, in}  ->
						Reason;
				 _         ->
						""
			     end,
		case NewState of
		    none ->
					{none, AutoReply};
		    {none, none} when Item#roster.subscription == none,
		                      Item#roster.ask == in ->
				delete_item(LUser, LServer, LJID),
				{none, AutoReply};
		    {Subscription, Pending} ->
				NewItem = Item#roster{subscription = Subscription,
					      ask = Pending,
					      askmessage = list_to_binary(AskMessage)},
				mnesia:write(NewItem),
				case mod_roster:roster_version_on_db(LServer) of
					true ->
						mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
					false ->
						ok
				end,
				{{push, NewItem}, AutoReply}
		end
	end,
    mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


update_table() ->
    Fields = record_info(fields, roster),
    case mnesia:table_info(roster, attributes) of
		Fields ->
			ok;
		[uj, user, jid, name, subscription, ask, groups, xattrs, xs] ->
			convert_table1(Fields);
		[usj, us, jid, name, subscription, ask, groups, xattrs, xs] ->
			convert_table2(Fields);
		_ ->
			?INFO_MSG("Recreating roster table", []),
			mnesia:transform_table(roster, ignore, Fields)
    end.


%% Convert roster table to support virtual host
convert_table1(Fields) ->
    ?INFO_MSG("Virtual host support: converting roster table from "
	      "{uj, user, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    Host = ?MYNAME,
    {atomic, ok} = mnesia:create_table(
		     mod_roster_tmp_table,
		     [{disc_only_copies, [node()]},
		      {type, bag},
		      {local_content, true},
		      {record_name, roster},
		      {attributes, record_info(fields, roster)}]),
    mnesia:del_table_index(roster, user),
    mnesia:transform_table(roster, ignore, Fields),
    F1 = fun() ->
		 mnesia:write_lock_table(mod_roster_tmp_table),
		 mnesia:foldl(
		   fun(#roster{usj = {U, JID}, us = U} = R, _) ->
			   mnesia:dirty_write(
			     mod_roster_tmp_table,
			     R#roster{usj = {U, Host, JID},
				      us = {U, Host}})
		   end, ok, roster)
	 end,
    mnesia:transaction(F1),
    mnesia:clear_table(roster),
    F2 = fun() ->
		 mnesia:write_lock_table(roster),
		 mnesia:foldl(
		   fun(R, _) ->
			   mnesia:dirty_write(R)
		   end, ok, mod_roster_tmp_table)
	 end,
    mnesia:transaction(F2),
    mnesia:delete_table(mod_roster_tmp_table).


%% Convert roster table: xattrs fields become
convert_table2(Fields) ->
    ?INFO_MSG("Converting roster table from "
	      "{usj, us, jid, name, subscription, ask, groups, xattrs, xs} format", []),
    mnesia:transform_table(roster, ignore, Fields).

