-module(ct_expand).
-export([parse_transform/2]).


parse_transform(Forms, Options) ->
    {NewForms,_} = 
        parse_trans:depth_first(fun xform_fun/4, [], Forms, Options),
    parse_trans:revert(NewForms).


xform_fun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
        {?MODULE, {term, 1}} ->
            Args = erl_syntax:application_arguments(Form),
            RevArgs = parse_trans:revert(Args),
            case erl_eval:exprs(RevArgs, []) of
                {value, Value,[]} ->
                    {erl_syntax:abstract(Value), Acc};
                Other ->
                    parse_trans:error(cannot_evaluate,?LINE,
                                      [{expr, RevArgs},
                                       {error, Other}])
            end;
        _ ->
            {Form, Acc}
    end;
xform_fun(_, Form, _Ctxt, Acc) ->
    {Form, Acc}.
  
