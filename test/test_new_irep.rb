# Make irep
iseq = [41]                # return
irep = Irep.new_irep(iseq, [], [], 0, 0)
irep.to_proc.call
