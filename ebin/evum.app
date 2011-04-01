{application, evum,
    [
    {description, "Linux VM in an Erlang VM"},
    {vsn, "0.01"},
    {modules, [
        evum,
        evum_mcons,
        evum_ctl,
        evum_switch,
        mcons,
        evm,
        evm_cfg
            ]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
            ]},
    {env, []}
    ]}.

