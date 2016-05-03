define mbt
  set $p = mrb->c->ci
  while ($p > mrb->c->cibase)
    if (($p->proc->flags & 128) != 0 )
      printf "0x%x   C ", $p
      output $p->proc->body.func
      printf "\n"
    else
      set $irep = $p->proc->body.irep
      set $mid = $p->mid
      set $method_name = mrb_sym2name(mrb, $mid)
      set $filename = $irep->filename
      set $lines = $irep->lines
      if ($method_name) 
	if ($filename && $lines)
	  if ($p == mrb->c->ci)
	    set $lineno = $lines[pc - $irep->iseq]
	  else
	    set $lineno = $lines[($p + 1)->pc - $irep->iseq]
	  end
          printf "0x%x MRUBY %s : %s:%d\n", $p, $method_name, $filename, $lineno
	else
          printf "0x%x MRUBY %s : \n", $p, $method_name
	end
      else
        printf "0x%x MRUBY    : \n", $p
      end
    end
    set $p = $p - 1
  end
end