# Ollama Translator

An Erlang library for translating text using the Ollama API.


[![Hex.pm](https://img.shields.io/hexpm/v/ollama_translator.svg)](https://hex.pm/packages/ollama_translator)
[![Hex Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/ollama_translator)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

## Features

- 🌐 Translate text to any target language
- ⚙️ Configurable via environment variables or runtime configuration
- 🔧 Support for custom Ollama endpoints and models
- 📝 Customizable prompt templates
- 🎯 Simple and focused API

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {ollama_translator, "0.1.0"}
]}.
```

## Quick Start

```erlang
% Start your Erlang shell
$ rebar3 shell

% Simple translation
1> ollama_translator:translate("French", "Hello world").
{ok, <<"Bonjour le monde">>}

2> ollama_translator:translate("Spanish", "How are you?").
{ok, <<"¿Cómo estás?">>}

3> ollama_translator:translate(<<"German">>, "Good morning").
{ok, <<"Guten Morgen">>}

```

## Configuration

### Environment Variables

Set these environment variables to override defaults:

```bash
export OLLAMA_ENDPOINT="http://localhost:11434/api/generate"  # Default
export OLLAMA_MODEL="phi3"                                   # Default  
export OLLAMA_PROMPT="Translate the following text to ~s. Only return the translation, no explanations:\n\n~s"  # Default
```

### Runtime Configuration

```erlang
% Custom configuration
Config = #{
    endpoint => "http://my-ollama-server:11434/api/generate",
    model => <<"llama2">>,
    prompt_template => "Traduis ce texte en ~s :\n\n~s"
},

% Use custom config
ollama_translator:translate("Italian", "Hello world", Config).

% Modify default config partially
BaseConfig = ollama_translator:get_env_config(),
MyConfig = BaseConfig#{model => <<"codellama">>},
ollama_translator:translate("Portuguese", "Good bye", MyConfig).
```

## API Reference

### Main Functions

#### `translate/2,3`
```erlang
translate(TargetLanguage, Text) -> {ok, Translation} | {error, Reason}.
translate(TargetLanguage, Text, Config) -> {ok, Translation} | {error, Reason}.
```
Translates text to the specified target language.

- `TargetLanguage` can be a string or binary (e.g., `"French"`, `<<"Spanish">>`)
- `Text` is the text to translate as a string
- Returns the translation as a binary

### Configuration Functions

#### `default_config/0`
```erlang
default_config() -> Config.
```
Returns hardcoded default configuration.

#### `get_env_config/0`
```erlang
get_env_config() -> Config.
```
Returns configuration with environment variable overrides.

### Utility Functions

#### `print_result/1`
```erlang
print_result(Result) -> ok | error.
```
Pretty prints translation results to stdout.

## Configuration Map

```erlang
Config = #{
    endpoint => "http://localhost:11434/api/generate",    % Ollama API endpoint
    model => <<"phi3">>,                                 % Model name as binary
    prompt_template => "Translate text to ~s:\n\n~s"     % Prompt with 2 ~s placeholders
}.
```

**Note**: The prompt template must contain exactly 2 `~s` placeholders:
1. First `~s` for the target language
2. Second `~s` for the text to translate

## Prerequisites

- Erlang/OTP 21+
- Running Ollama instance
- `jsx` dependency for JSON handling
- `inets` application for HTTP requests

## Dependencies

This library requires:
- `jsx` for JSON encoding/decoding
- `inets` (part of OTP) for HTTP client

Add to your `rebar.config`:

```erlang
{deps, [
    ollama_translator
]}.
```

## Examples

### Basic Usage

```erlang
% Various languages
ollama_translator:translate("French", "Hello").
ollama_translator:translate("Spanish", "Good morning").
ollama_translator:translate("German", "How are you?").
ollama_translator:translate("Italian", "Thank you very much").
ollama_translator:translate("Portuguese", "See you later").
ollama_translator:translate("Russian", "Good night").
ollama_translator:translate("Chinese", "Welcome").
ollama_translator:translate("Japanese", "Excuse me").
```

### Advanced Usage

```erlang
% Custom model for better translations
Config = #{model => <<"llama2">>},
ollama_translator:translate("French", "The weather is beautiful today", Config).

% Custom prompt for formal translation
FormalConfig = #{
    prompt_template => "Provide a formal translation of this text to ~s:\n\n~s"
},
ollama_translator:translate("Japanese", "Could you help me please?", FormalConfig).

% Technical translation
TechConfig = #{
    model => <<"codellama">>,
    prompt_template => "Translate this technical text to ~s, keeping technical terms:\n\n~s"
},
ollama_translator:translate("German", "Database connection failed", TechConfig).
```

### Error Handling

```erlang
case ollama_translator:translate("French", "Hello world") of
    {ok, Translation} ->
        io:format("Translation: ~s~n", [Translation]);
    {error, {ollama_error, StatusCode, Body}} ->
        io:format("Ollama API error ~p: ~s~n", [StatusCode, Body]);
    {error, {request_failed, Reason}} ->
        io:format("Request failed: ~p~n", [Reason]);
    {error, Reason} ->
        io:format("Other error: ~p~n", [Reason])
end.
```

### Batch Translation

```erlang
% Translate multiple texts
Texts = ["Hello", "Good morning", "Thank you", "Goodbye"],
Translations = [ollama_translator:translate("Spanish", Text) || Text <- Texts],
[ollama_translator:print_result(Result) || Result <- Translations].
```

## Language Examples

The library supports any language that your Ollama model can handle. Common examples:

- **European**: `"French"`, `"Spanish"`, `"German"`, `"Italian"`, `"Portuguese"`, `"Dutch"`, `"Russian"`
- **Asian**: `"Chinese"`, `"Japanese"`, `"Korean"`, `"Thai"`, `"Vietnamese"`
- **Other**: `"Arabic"`, `"Hebrew"`, `"Hindi"`, `"Turkish"`, `"Polish"`

You can also be more specific: `"Mexican Spanish"`, `"Brazilian Portuguese"`, `"Simplified Chinese"`, etc.

## Development

```bash
# Clone the repository
git clone https://github.com/roquess/ollama_translator_erlang.git
cd ollama_translator

# Install dependencies and compile
rebar3 compile

# Run tests (if available)
rebar3 eunit

# Start shell for testing
rebar3 shell
```

## Integration with Other Libraries

Works well with other text processing libraries:

```erlang
% With ollama_summarizer for translate summaries
{ok, Summary} = ollama_summarizer:summarize_url("https://example.com"),
{ok, Translation} = ollama_translator:translate("French", binary_to_list(Summary)).
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Changelog

### 0.1.0
- Initial release
- Basic text translation
- Environment variable configuration
- Custom configuration support
- Support for string and binary language input
