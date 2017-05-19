Begin
	Var x End
	Function f Paras Begin
		Function g Paras Begin
			Print x 
		End
		Call g Argus End
		Assign x 1
		Call g Argus End
		Var x End
		Assign x 2
		Call g Argus End
	End
	Assign x 0
	Call f Argus End
End