{application, libdist_python,
 [
  {description, "A Python wrapper around libdist"},
  {vsn, "0.1"},
  {modules, [
      libdist_python_app,
      libdist_python_sup,
      libdist_python_server,
      libdist_python_client
      ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { libdist_python_app, []}},
  {env, []}
 ]}.
