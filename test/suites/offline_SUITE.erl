-module(offline_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, presence}].

groups() ->
    [{presence, [{repeat_until_any_ok, 5}], [
        negative_presence_no_mod_offline,
        negative_presence
        ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

negative_presence_no_mod_offline(Config) ->
    escalus:story(Config, [1, 1], fun(Mary, Jane) ->

        Msg = "Hi, Jane!",
        BareJane = bare_jid(Jane),
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        escalus_assert:is_chat_message(Msg,
            escalus_client:wait_for_stanza(Jane)),

        %% set negative presence
        NegativePresence = prioritized_presence(available, -1),
        escalus_client:send(Jane, NegativePresence),
        %escalus_utils:log_stanzas("Negative presence", [NegativePresence]),
        escalus_assert:is_presence_stanza(
            escalus_client:wait_for_stanza(Jane)),

        %% chat again
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),

        %% Mary should receive service-unavailable
        Error = escalus_client:wait_for_stanza(Mary),
        escalus_utils:log_stanzas("Should be service-unavailable", [Error]),
        escalus_assert:is_error(Error, <<"cancel">>,
            'service-unavailable'),

        %% Jane should not receive message
        timer:sleep(500),
        escalus_assert:has_no_stanzas(Jane)

        end).

negative_presence(Config) ->
    escalus:story(Config, [1, 1], fun(Mary, Jane) ->

        %% No idea why, but when mod_offline is loaded more presences fly
        %% around. Maybe more than usual are sent, maybe they just aren't
        %% caught by escalus:story - I don't know.
        %% EDIT: This is even stranger. After changing the running
        %% discipline for the test group, these stanzas seem to disappear.
        %UselessPresence1 = escalus_client:wait_for_stanza(Jane),
        %escalus_utils:log_stanzas("Should be presence", [UselessPresence1]),
        %UselessPresence2 = escalus_client:wait_for_stanza(Mary),
        %escalus_utils:log_stanzas("Should be presence", [UselessPresence2]),

        Msg = "Hi, Jane!",
        BareJane = bare_jid(Jane),
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        escalus_assert:is_chat_message(Msg,
            escalus_client:wait_for_stanza(Jane)),

        %% set negative presence
        NegativePresence = prioritized_presence(available, -1),
        escalus_client:send(Jane, NegativePresence),
        %escalus_utils:log_stanzas("Negative presence", [NegativePresence]),
        escalus_assert:is_presence_stanza(
            escalus_client:wait_for_stanza(Jane)),

        %% Jane should not receive message
        escalus_client:send(Mary, chat_to_jid(BareJane, Msg)),
        timer:sleep(500),
        escalus_assert:has_no_stanzas(Jane),

        %% set non-negative presence
        timer:sleep(500),
        escalus_client:send(Jane, escalus_stanza:presence(available)),
        escalus_assert:is_presence_stanza(escalus_client:wait_for_stanza(Jane)),

        %% Mary should not receive anything
        escalus_assert:has_no_stanzas(Mary),

        %% Jane should receive delayed message
        DelayedMessage = escalus_client:wait_for_stanza(Jane),
        escalus_utils:log_stanzas("Delayed message", [DelayedMessage]),
        escalus_assert:is_chat_message(Msg, DelayedMessage),
        true = is_delayed(DelayedMessage)

        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

bare_jid(Client) ->
    hd(string:tokens(binary_to_list(Client#client.jid), "/")).

prioritized_presence(Type, Priority) ->
    exmpp_presence:set_priority(
        escalus_stanza:presence(Type),
        Priority).

chat_to_jid(ClientJid, Msg) ->
    exmpp_stanza:set_recipient(
        exmpp_message:chat(Msg), 
        ClientJid).

is_delayed(Stanza) ->
    true == exmpp_xml:has_element(Stanza, 'delay').
