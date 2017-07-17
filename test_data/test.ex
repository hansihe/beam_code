defmodule Test do

  #def add(a, b) do
  #  a + b
  #end

  def add_catch(a, b) do
    #try do
    #  a + b
    #rescue
    #  ArithmeticError -> :error
    #end
    c = case a do
      {:woo, 12} -> 9
      {:hoo, 11} -> 8
    end
    c + b
  end

  #def complex(l) do
  #  a = 0
  #  for a <- l do
  #    for b <- a do
  #      for c <- b do
  #        for d <- c do
  #          IO.inspect {d, a}
  #        end
  #      end
  #    end
  #  end
  #end

  #def term_construct(a, b, c) do
  #  {c, [a, b, c], 1, 2, 3}
  #end

  def big_bif do
    :erlang.send_after(10, self, :woohoo, [])
  end

  #def simple_match(a) do
  #  case a do
  #    {:some, thing} -> thing
  #    {:else, thing} -> is_integer(thing)
  #    {:woo, thing} -> thing
  #  end
  #end

  #def multi_match(a) do
  #  case a do
  #    {:some, thing} -> thing
  #    woo when is_integer(woo) -> woo + 5
  #    hoo when is_binary(hoo) -> hoo
  #  end
  #end

  #def tuple_elem(a, b) do
  #  elem(a, b)
  #end

  def lambda(bound) do
    fn (a) -> bound + a end
  end

end
