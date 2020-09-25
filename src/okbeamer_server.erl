%%%-------------------------------------------------------------------
%%% @doc
%%%     okbeamer server
%%% @end
%%%-------------------------------------------------------------------
-module(okbeamer_server).
-author("Igor Kopestenski").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([leds_on/0]).
-export([leds_on/1]).
-export([leds_off/0]).
-export([leds_off/1]).
-export([light_meter_mode/1]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         terminate/2 ,
         code_change/3]).

-define(SERVER , ?MODULE).
-define(gpio1_pins , [gpio1_1, gpio1_2, gpio1_3, gpio1_4]).
-define(gpio1_pins_4 , [gpio1_1, gpio1_2, gpio1_3, gpio1_4]).
-define(gpio1_pins_3 , [gpio1_1, gpio1_2, gpio1_3]).
-define(gpio1_pins_2 , [gpio1_1, gpio1_2]).
-define(gpio1_pins_1 , [gpio1_1]).

-define(gpio2_pins , [gpio2_1, gpio2_2, gpio2_3, gpio2_4]).
-define(gpio2_pins_4 , [gpio2_1, gpio2_2, gpio2_3, gpio2_4]).
-define(gpio2_pins_3 , [gpio2_1, gpio2_2, gpio2_3]).
-define(gpio2_pins_2 , [gpio2_1, gpio2_2]).
-define(gpio2_pins_1 , [gpio2_1]).


-define(gpio_pins_all , ?gpio1_pins ++ ?gpio2_pins).

-record(state , {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok , Pid :: pid()} | ignore | {error , Reason :: term()}).
start_link() ->
    gen_server:start_link({local , ?SERVER} , ?MODULE , [] , []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok , State :: #state{}} | {ok , State :: #state{} , timeout() | hibernate} |
    {stop , Reason :: term()} | ignore).
init([]) ->
    grisp_led:color(1, green),

    %% Configure slot for Board -> Pmod communication with pins set to 0
    grisp_gpio:configure_slot(gpio1, {output_0, output_0, output_0, output_0}),
    grisp_gpio:configure_slot(gpio2, {output_0, output_0, output_0, output_0}),
    {ok , #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term() , From :: {pid() , Tag :: term()} ,
                  State :: #state{}) ->
                     {reply , Reply :: term() , NewState :: #state{}} |
                     {reply , Reply :: term() , NewState :: #state{} , timeout() | hibernate} |
                     {noreply , NewState :: #state{}} |
                     {noreply , NewState :: #state{} , timeout() | hibernate} |
                     {stop , Reason :: term() , Reply :: term() , NewState :: #state{}} |
                     {stop , Reason :: term() , NewState :: #state{}}).
handle_call(_Request , _From , State) ->
    {reply , ok , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term() , State :: #state{}) ->
    {noreply , NewState :: #state{}} |
    {noreply , NewState :: #state{} , timeout() | hibernate} |
    {stop , Reason :: term() , NewState :: #state{}}).
handle_cast(_Request , State) ->
    {noreply , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term() , State :: #state{}) ->
    {noreply , NewState :: #state{}} |
    {noreply , NewState :: #state{} , timeout() | hibernate} |
    {stop , Reason :: term() , NewState :: #state{}}).
handle_info(_Info , State) ->
    {noreply , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown , term()} | term()) ,
                State :: #state{}) -> term()).
terminate(_Reason , _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down , term()} , State :: #state{} ,
                  Extra :: term()) ->
                     {ok , NewState :: #state{}} | {error , Reason :: term()}).
code_change(_OldVsn , State , _Extra) ->
    {ok , State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Switches Pmod_8LD LEDs on
%%
%% @end
%%--------------------------------------------------------------------
leds_on() ->
    _ = [ grisp_gpio:set(Pin) || Pin <- ?gpio_pins_all ].
leds_on(Pins) when is_list(Pins) ->
    _ = [ grisp_gpio:set(Pin) || Pin <- Pins ];
leds_on(N) when is_integer(N) ; N >= 0 ; N =< 8 ->
    _ = [ grisp_gpio:set(pin_at_index(Pin)) || Pin <- lists:seq(1, N) ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Switches Pmod_8LD LEDs off
%%
%% @end
%%--------------------------------------------------------------------
leds_off() ->
    _ = [ grisp_gpio:clear(Pin) || Pin <- ?gpio_pins_all ].
leds_off(Pins) when is_list(Pins) ->
    _ = [ grisp_gpio:clear(Pin) || Pin <- Pins ];    
leds_off(N) when is_integer(N) ; N >= 0 ; N =< 8 ->
    _ = [ grisp_gpio:set(pin_at_index(Pin)) || Pin <- lists:seq(1, N) ].
    
leds_split(0, 4) -> leds_off();
leds_split(1, 3) -> leds_on(?gpio1_pins_1), leds_off(?gpio1_pins_3);
leds_split(2, 2) -> leds_on(?gpio1_pins_2), leds_off(?gpio1_pins_2);
leds_split(3, 1) -> leds_on(?gpio1_pins_3), leds_off(?gpio1_pins_1);
leds_split(4, 0) -> leds_on().

light_meter_mode(0) ->
    leds_off();
light_meter_mode(N) ->
    leds_off(),
    light_meter(pmod_als:percentage()),
    % timer:sleep(10),
    light_meter_mode(N - 1).

% light_meter(Percentage) when Percentage >= 0 andalso Percentage < 20 -> 
%     leds_off();
% light_meter(Percentage) when Percentage >= 20 andalso Percentage < 40 -> 
%     grisp_gpio:set(gpio1_1);
% light_meter(Percentage) when Percentage >= 40 andalso Percentage < 60 -> 
%     leds_on(?gpio1_pins_2);
% light_meter(Percentage) when Percentage >= 60 andalso Percentage < 80 -> 
%     leds_on(?gpio1_pins_3);
% light_meter(Percentage) when Percentage >= 80 andalso Percentage =< 100 -> 
%     leds_on().

light_meter(Percentage) when Percentage >= 0 andalso Percentage < 10 -> 
    leds_off();
light_meter(Percentage) when Percentage >= 10 andalso Percentage < 20 -> 
    leds_on(1);
light_meter(Percentage) when Percentage >= 20 andalso Percentage < 30 -> 
    leds_on(2);
light_meter(Percentage) when Percentage >= 30 andalso Percentage < 40 -> 
    leds_on(3);
light_meter(Percentage) when Percentage >= 40 andalso Percentage < 50 -> 
    leds_on(4);
light_meter(Percentage) when Percentage >= 50 andalso Percentage < 60 -> 
    leds_on(5);
light_meter(Percentage) when Percentage >= 60 andalso Percentage < 75 -> 
    leds_on(6);
light_meter(Percentage) when Percentage >= 75 andalso Percentage < 88 -> 
    leds_on(7);
light_meter(Percentage) when Percentage >= 88 andalso Percentage =< 100 -> 
    leds_on().
    % leds_on(8);

pin_at_index(1) -> gpio2_1;
pin_at_index(2) -> gpio2_2;
pin_at_index(3) -> gpio2_3;
pin_at_index(4) -> gpio2_4;
pin_at_index(5) -> gpio1_1;
pin_at_index(6) -> gpio1_2;
pin_at_index(7) -> gpio1_3;
pin_at_index(8) -> gpio1_4;
pin_at_index(Arg) -> undefined.

% adding_handler(#{config := RawConfig} = LoggerConfig) ->
%     DgramConfig = maps:merge(?DEFAULT_CONFIG, RawConfig),
%     Parent = self(),
%     case verify_config([host, port, measurement], DgramConfig) of
%         ok ->
%             Pid = spawn(fun() -> Parent ! gen_udp:open(0), 
%                                  receive stop -> ok end end),
%             {ok, LoggerConfig#{config => DgramConfig#{
%                 sock => receive {ok, Socket} -> Socket end,
%                 pid => Pid,
%                 measurement => to_binary(maps:get(measurement, DgramConfig))
%             }}};
%         Error ->
%             Error
%     end.