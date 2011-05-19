{application, orologio,
  [{description, "Monitoring system"},
   {vsn, "0.0.1"},
   {modules, [orologio, orologio_app, orologio_sup, orologio_fetch_sup,orologio_graph_sup, mod_rrdtool, mod_config_file]},
   {applications, [kernel, stdlib, sasl]},
   {mod, {orologio_app, []}}
  ]
}.
