-module(ollama_translator).
-author("Steve Roques").

%% Public API
-export([
    translate/2, translate/3,
    default_config/0,
    get_env_config/0
]).

-type config() :: map().
-type translate_result() :: {ok, binary()} | {error, term()}.
-type language() :: string() | binary().

%% =============================================================================
%% Main translation functions
%% =============================================================================

%% @doc
%% Translate text to the target language using environment/default config.
-spec translate(language(), string()) -> translate_result().
translate(TargetLang, Text) ->
    translate(TargetLang, Text, get_env_config()).

%% @doc
%% Translate text to the target language using a custom config.
-spec translate(language(), string(), config()) -> translate_result().
translate(TargetLang, Text, Config) ->
    %% Ensure target language is a string
    TargetLangStr = case TargetLang of
        Lang when is_binary(Lang) -> binary_to_list(Lang);
        Lang when is_list(Lang) -> Lang
    end,
    %% Build the prompt using the template from config or default
    PromptTemplate = maps:get(prompt_template, Config, default_prompt_template()),
    Prompt = ollama_handler:format_prompt(PromptTemplate, [TargetLangStr, Text]),
    %% Call the handler and return its result directly
    ollama_handler:generate(Prompt, Config).

%% =============================================================================
%% Configuration helpers
%% =============================================================================

%% @doc
%% Returns the default config (handler defaults + our default prompt template).
-spec default_config() -> config().
default_config() ->
    maps:merge(
        ollama_handler:default_config(),
        #{prompt_template => default_prompt_template()}
    ).

%% @doc
%% Returns config from environment variables, falling back to defaults.
-spec get_env_config() -> config().
get_env_config() ->
    maps:merge(
        ollama_handler:get_env_config(),
        #{prompt_template => os:getenv("OLLAMA_PROMPT", default_prompt_template())}
    ).

%% @doc
%% The default prompt template for translation.
-spec default_prompt_template() -> string().
default_prompt_template() ->
    "Translate the following text to ~s. Only return the translation, no explanations:\n\n~s".

