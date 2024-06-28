rebar3_tele
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_tele, {git, "https://host/user/rebar3_tele.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_tele
    ===> Fetching rebar3_tele
    ===> Compiling rebar3_tele
    <Plugin Output>
