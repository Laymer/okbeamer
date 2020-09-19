### Error while deploying

```
DEBUG=1 rebar3 grisp deploy -n okbeamer -v 0.1.0
===> Load global config file /Users/laymer/.config/rebar3/rebar.config
===> Rebar3 detected a lock file from a newer version. It will be loaded in compatibility mode, but important information may be missing or lost. It is recommended to upgrade Rebar3.
===> 22.0.4 satisfies the requirement for minimum OTP version 18
===> Expanded command sequence to be run: []
===> Provider: {default,do}
===> Expanded command sequence to be run: [{default,app_discovery},
                                                  {default,install_deps},
                                                  {default,lock},
                                                  {default,compile},
                                                  {grisp,deploy}]
===> Provider: {default,app_discovery}
===> Evaluating config script "/Users/laymer/Dev/OTP22-0-4/okbeamer/_build/default/lib/grisp/rebar.config.script"
===> Provider: {default,install_deps}
===> Verifying dependencies...
===> sh info:
	cwd: "/Users/laymer/Dev/OTP22-0-4/okbeamer"
	cmd: git --version

===> 	opts: [{return_on_error,true},{use_stdout,false}]

===> Port Cmd: git --version
Port Opts: [exit_status,{line,16384},use_stdio,stderr_to_stdout,hide,eof]

===> sh info:
	cwd: "/Users/laymer/Dev/OTP22-0-4/okbeamer"
	cmd: git rev-parse --short=7 -q HEAD

===> 	opts: [{cd,"/Users/laymer/Dev/OTP22-0-4/okbeamer/_build/default/lib/epmd"}]

===> Port Cmd: git rev-parse --short=7 -q HEAD
Port Opts: [{cd,"/Users/laymer/Dev/OTP22-0-4/okbeamer/_build/default/lib/epmd"},
            exit_status,
            {line,16384},
            use_stdio,stderr_to_stdout,hide,eof]

===> Comparing git ref 4d1a595 with 4d1a595
===> Provider: {default,lock}
===> Provider: {default,compile}
===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", pre_hooks, compile) -> no hooks defined

===> Compiling okbeamer
===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", pre_hooks, compile) -> no hooks defined

===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", pre_hooks, erlc_compile) -> no hooks defined

===> erlopts [debug_info]
===> files to compile ["/Users/laymer/Dev/OTP22-0-4/okbeamer/src/okbeamer_server.erl",
                              "/Users/laymer/Dev/OTP22-0-4/okbeamer/src/okbeamer.erl",
                              "/Users/laymer/Dev/OTP22-0-4/okbeamer/src/okbeamer_sup.erl",
                              "/Users/laymer/Dev/OTP22-0-4/okbeamer/src/okbeamer_app.erl"]
===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", post_hooks, erlc_compile) -> no hooks defined

===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", pre_hooks, app_compile) -> no hooks defined

===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", post_hooks, app_compile) -> no hooks defined

===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", post_hooks, compile) -> no hooks defined

===> run_hooks("/Users/laymer/Dev/OTP22-0-4/okbeamer", post_hooks, compile) -> no hooks defined

===> Provider: {grisp,deploy}
===> [rebar3_grisp] {otp_type,"bb415d583ab2fc47d41963909914d7b3e2b664d49e4587342a2d5cf1957da018",
                                     package}
* Downloading pre-built OTP package (bb415d58)
===> [rebar3_grisp] {package,
                               {current_etag,
                                   "\"e467c5698efcd5b01c09fe64f50fb735-5\""}}
===> [rebar3_grisp] {package,
                               {deleting_tmp_file,
                                   "/Users/laymer/Library/Caches/grisp/packages/otp/grisp_base/grisp_otp_build_22.0_bb415d583ab2fc47d41963909914d7b3e2b664d49e4587342a2d5cf1957da018/grisp_otp_build_22.0_bb415d583ab2fc47d41963909914d7b3e2b664d49e4587342a2d5cf1957da018.temp"}}
===> Uncaught error in rebar_core. Run with DEBUG=1 to see stacktrace or consult rebar3.crashdump
===> Uncaught error: badarg
===> Stack trace to the error location:
[{erlang,'++',
         [[47|grisp_base],
          "/otp/grisp_otp_build_22.0_bb415d583ab2fc47d41963909914d7b3e2b664d49e4587342a2d5cf1957da018.tar.gz"],
         []},
 {lists,append,1,[{file,"lists.erl"},{line,127}]},
 {lists,append,1,[{file,"lists.erl"},{line,127}]},
 {string,join,2,[{file,"string.erl"},{line,2244}]},
 {grisp_tools_deploy,package_download,1,
                     [{file,"/Users/laymer/.cache/rebar3/plugins/grisp_tools/src/grisp_tools_deploy.erl"},
                      {line,88}]},
 {lists,foldl,3,[{file,"lists.erl"},{line,1263}]},
 {rebar3_grisp_deploy,do,1,
                      [{file,"/Users/laymer/.cache/rebar3/plugins/rebar3_grisp/src/rebar3_grisp_deploy.erl"},
                       {line,69}]},
 {rebar_core,do,2,
             [{file,"/tmp/cirrus-ci-build/src/rebar_core.erl"},{line,154}]}]
===> When submitting a bug report, please include the output of `rebar3 report "your command"`
```

### Rebar3 Report
```
rebar3 report "rebar3 grisp deploy -n okbeamer -v 0.1.0"
===> Rebar3 detected a lock file from a newer version. It will be loaded in compatibility mode, but important information may be missing or lost. It is recommended to upgrade Rebar3.
Rebar3 report
 version 3.13.2
 generated at 2020-09-19T17:50:47+00:00
=================
Please submit this along with your issue at https://github.com/erlang/rebar3/issues (and feel free to edit out private information, if any)
-----------------
Task: rebar3
Entered as:
  rebar3 grisp deploy -n okbeamer -v 0.1.0
-----------------
Operating System: x86_64-apple-darwin19.5.0
ERTS: Erlang/OTP 22 [erts-10.4.3] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [hipe]
Root Directory: /Users/laymer/.asdf/installs/erlang/22.0.4
Library directory: /Users/laymer/.asdf/installs/erlang/22.0.4/lib
-----------------
Loaded Applications:
bbmustache: 1.6.1
certifi: 2.5.1
cf: 0.2.2
common_test: 1.17.3
compiler: 7.4.2
crypto: 4.5.1
cth_readable: 1.4.6
dialyzer: 4.0.1
edoc: 0.11
erlware_commons: 1.3.1
eunit: 2.3.7
eunit_formatters: 0.5.0
getopt: 1.0.1
hipe: 3.19
inets: 7.0.8
kernel: 6.4.1
providers: 1.8.1
public_key: 1.6.7
relx: 3.33.0
sasl: 3.4
snmp: 5.3
ssl_verify_fun: 1.1.5
stdlib: 3.9.2
syntax_tools: 2.2
tools: 3.2

-----------------
Escript path: undefined
Providers:
  app_discovery as build clean compile compile cover ct cut deploy deps dialyzer do docs edoc escriptize eunit get-deps help install install_deps key list lock new owner path pkgs publish release relup repo report repos retire revert search shell state tar tree unlock update upgrade upgrade upgrade user version version xref
```