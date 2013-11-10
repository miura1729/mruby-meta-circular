# Make irep
irep = Irep.new_irep
irep.iseq = [41]                # return
irep.to_proc.call
