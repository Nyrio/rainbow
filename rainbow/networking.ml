(* ideas:
    * state: either working or idle. If idle, see if other agents are
      idle as well. If true send everyone shutdown message and exit, if false
      wait for some node to wake you up or shut you down.
      wake_up node: wake_up the node.
    * 
    *)
module type Agent = sig
    type config  (* a datastructure that will be given to init an agent *)
    type public  (* a datastructure publicly available for other nodes *)
    type locals  (* a datastructure privately available *)

    val bootstrap : unit -> config
    val init : config -> (public, locals)
    val work : locals -> public -> public list -> locals
    val finish : locals -> unit
end
