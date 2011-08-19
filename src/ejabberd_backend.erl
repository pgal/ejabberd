%%%-------------------------------------------------------------------
%%% File : ejabberd_config.erl
%%% Description :
%%% Author : Radoslaw Szymczyszyn <r.szymczyszyn@gmail.com>
%%%-------------------------------------------------------------------
-module(ejabberd_backend).

-export([get/2,
         load/2]).

%% Get DB backend per Host/Module combination.
get(Host, Module) ->
    %% extend if/when more backends appear (see also _1_)
    MnesiaBackend = list_to_atom(atom_to_list(Module) ++ "_mnesia"),
    OdbcBackend = list_to_atom(atom_to_list(Module) ++ "_odbc"),
    RiakBackend = list_to_atom(atom_to_list(Module) ++ "_riak"),
    
    Modules = ejabberd_config:get_local_option({modules, Host}),
    Select = fun({Mod,_}, Acc) ->
        %% ASSUMPTION: either mod_something or mod_something_odbc
        %% (or some other backend) is used, never both/all
        case Mod of
            %% _1_ add cases for more backends
            MnesiaBackend -> mnesia;
            OdbcBackend -> odbc;
            RiakBackend -> riak;
            _ -> Acc
        end
    end,
    lists:foldl(Select, none, Modules).

%% load(some_mod, some_atom) compiles and loads into the VM a module with
%% following content:
%%   -module(some_mod_backend).
%%   -compile([export_all]).
%%   backend() -> some_atom.
%% What for? It allows for runtime initilization of extremely fast
%% global state stored in code memory. Intended use is dispatching backend
%% specific mod_something callbacks.
load(Module, Backend) ->
    ModName = list_to_atom(lists:concat([Module, "_backend"])),
    Code = lists:concat([
        "-module(", ModName,"). ",
        "-compile([export_all]). ",
        "backend() -> ", Backend, ". "]),
    {ModName, Binary} = dynamic_compile:from_string(Code),
    code:load_binary(ModName, lists:concat([ModName, ".erl"]), Binary).
