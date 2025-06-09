-module(ollama_translator).

%% Ollama Translator Library
%% This library provides functions to translate text using Ollama API.
%% It supports both default configuration and custom configuration per request.
%% Environment variables can be used to override default settings.

-author("Steve Roques").

%% Public API
-export([
    translate/2, translate/3,
    print_result/1,
    default_config/0,
    get_env_config/0
]).

%% Default configuration constants
-define(DEFAULT_ENDPOINT, "http://localhost:11434/api/generate").
-define(DEFAULT_MODEL, <<"phi3">>).
-define(DEFAULT_PROMPT_TEMPLATE, "Translate the following text to ~s. Only return the translation, no explanations:\n\n~s").

%% Types
-type config() :: #{
    endpoint => string(),
    model => binary(),
    prompt_template => string()
}.

-type translate_result() :: {ok, binary()} | {error, term()}.
-type language() :: string() | binary().

%% =============================================================================
%% Public API with default configuration
%% =============================================================================

%% Translate text to target language using default/environment configuration.
-spec translate(language(), string()) -> translate_result().
translate(TargetLang, Text) ->
    translate(TargetLang, Text, get_env_config()).

%% =============================================================================
%% Public API with custom configuration
%% =============================================================================

%% Translate text to target language using custom configuration.
%% Config map contains endpoint, model, and prompt_template.
-spec translate(language(), string(), config()) -> translate_result().
translate(TargetLang, Text, Config) ->
    Endpoint = maps:get(endpoint, Config, ?DEFAULT_ENDPOINT),
    Model = maps:get(model, Config, ?DEFAULT_MODEL),
    PromptTemplate = maps:get(prompt_template, Config, ?DEFAULT_PROMPT_TEMPLATE),
    
    % Convert target language to string if binary
    TargetLangStr = case TargetLang of
        Lang when is_binary(Lang) -> binary_to_list(Lang);
        Lang when is_list(Lang) -> Lang
    end,
    
    % Format the prompt with target language and text content
    Prompt = io_lib:format(PromptTemplate, [TargetLangStr, Text]),
    
    % Prepare JSON payload for Ollama API
    Payload = jsx:encode(#{
        <<"model">> => Model,
        <<"prompt">> => list_to_binary(Prompt),
        <<"stream">> => false
    }),
    
    % Make HTTP POST request to Ollama API
    Headers = [{"Content-Type", "application/json"}],
    case httpc:request(post, {Endpoint, Headers, "application/json", Payload}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            parse_ollama_response(Body);
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {ollama_error, StatusCode, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% =============================================================================
%% Configuration functions
%% =============================================================================

%% Get default hardcoded configuration.
-spec default_config() -> config().
default_config() ->
    #{
        endpoint => ?DEFAULT_ENDPOINT,
        model => ?DEFAULT_MODEL,
        prompt_template => ?DEFAULT_PROMPT_TEMPLATE
    }.

%% Get configuration from environment variables with fallback to defaults.
%% Environment variables:
%% - OLLAMA_ENDPOINT: Ollama API endpoint (default: http://localhost:11434/api/generate)
%% - OLLAMA_MODEL: Model name to use (default: phi3)
%% - OLLAMA_PROMPT: Prompt template with ~s placeholders (default: English translation prompt)
-spec get_env_config() -> config().
get_env_config() ->
    #{
        endpoint => os:getenv("OLLAMA_ENDPOINT", ?DEFAULT_ENDPOINT),
        model => list_to_binary(os:getenv("OLLAMA_MODEL", binary_to_list(?DEFAULT_MODEL))),
        prompt_template => os:getenv("OLLAMA_PROMPT", ?DEFAULT_PROMPT_TEMPLATE)
    }.

%% =============================================================================
%% Utility functions
%% =============================================================================

%% Print the result of a translation operation to stdout.
-spec print_result(translate_result()) -> ok | error.
print_result({ok, Text}) ->
    io:format("~s~n", [Text]),
    ok;
print_result({error, Reason}) ->
    io:format("Error: ~p~n", [Reason]),
    error.

%% =============================================================================
%% Private functions
%% =============================================================================

%% Parse JSON response from Ollama API.
parse_ollama_response(Body) ->
    try
        Json = jsx:decode(list_to_binary(Body)),
        case maps:get(<<"response">>, Json, undefined) of
            undefined ->
                {error, no_response_field};
            Response ->
                {ok, Response}
        end
    catch
        _:Error ->
            {error, {json_parse_error, Error}}
    end.
