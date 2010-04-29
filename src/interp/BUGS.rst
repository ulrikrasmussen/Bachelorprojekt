Exported continuations are leaking
----------------------------------
When calling a synchronous message on an external location, the return
continuation is marked as exported. For instance, when we desugar something
like this::

  { do call(x); ... }

where ``call`` is external, we currently generate something like this::

  def k<> |> ...
   in call<x,k>

Because the ``k`` gets exported, it can never be GC'ed. The solution is to
change the desguar module, so we get something like this::

  def k<> & incall<> |>
   in call<x,k> & incall<>

The ``incall`` message is only generated once, and the GC can therefore safely
unmark the ``k``.
