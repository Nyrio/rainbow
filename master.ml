module type Conf = sig
    val pw_charset: int
    val pw_len: int
end

type config = {
  pw_charset: string;
  pw_len: int;

  chain_len: int;
  chain_n: int;
  slave_n: int;
  max_chains_in_mem: int
}


let wait () =
  def x() & y()  = reply to x in x


let start conf =
  let slices = Util.create_slices conf.chain_n conf.max_chains_in_mem conf.slave_n in
  let () = Join.Ns.register Join.Ns.here "slices" (slices: string array) in
  def register_slave(slave)
