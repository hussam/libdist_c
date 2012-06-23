{application, libdist_c,
 [
  {description, "A C wrapper around libdist"},
  {vsn, "0.1"},
  {modules, [
      libdist_c_app,
      libdist_c_sup,
      libdist_c_server,
      libdist_c_client
      ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { libdist_c_app, []}},
  {env, []}
 ]}.
