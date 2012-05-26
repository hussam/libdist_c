{application, echo,
   [
      {description, "A simple echo server"},
      {vsn, "0.1"},
      {modules, [
         echo
         ]},
      {registered, []},
      {applications, [kernel, stdlib]}
   ]
}.
