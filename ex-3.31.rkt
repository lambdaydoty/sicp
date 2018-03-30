;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; before
THE AGENDA:


sum-wire 0 New-value = 0
carry-wire 0 New-value = 0'ok

THE AGENDA:
2.(<proc>)
3.(<proc> <proc> <proc> <proc>)
5.(<proc> <proc>)

'done

THE AGENDA:
2.(<proc>)
3.(<proc> <proc> <proc> <proc> <proc>)
5.(<proc> <proc> <proc>)


sum-wire 8 New-value = 1'done
'done

THE AGENDA:
11.(<proc>)
13.(<proc>)


carry-wire 11 New-value = 1
sum-wire 16 New-value = 0'done
> 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; after
THE AGENDA:

'ok

THE AGENDA:

'done

THE AGENDA:
3.(<proc>)
5.(<proc>)

'done
'done

THE AGENDA:
11.(<proc>)
13.(<proc>)


carry-wire 11 New-value = 1'done
> 
