source('tests/test_functions.R')

#TODO: Should think if this should be sourced here
# source('model_testing/data_sets.R')

needed_Finite <- function(obj){
	ret <- tryCatch(is.logical(obj),
			warning = function(w){
				print(w)
				quit("no",status = -1)
			},
			error = function(e){
				print(e)
				quit("no",status = -1)
			}
			)
	if(!ret) quit("no",status = -1)
	return(ret)
}

needed_input <- function(obj){
	ret <- tryCatch(is_valid_input(obj),
			warning = function(w){
				print(w)
				quit("no",status = -1)
			},
			error = function(e){
				print(e)
				quit("no",status = -1)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}

needed_fullname <- function(obj){
	ret <- tryCatch(is_valid_string(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}
needed_plotcolor <- function(obj){
	ret <- tryCatch(is_valid_string(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}
needed_methods <- function(obj){
	#TODO:
	#   User can mention many methods with prority order
	ret <- tryCatch(is_valid_string(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}

# needed_MLE <- function(model,obj){
# 	found_MLE <- tryCatch(as.logical(
# 						length(findFunction(
# 							paste(model,get(
# 								paste(model,"method",sep="_")),"MLE",sep="_")))),

# 			warning = function(w){
# 				print(w)
# 				#stop("Something went wrong!",model)
# 				return(FALSE)
# 			},
# 			error = function(e){
# 				print(typeof(e))
# 				#stop("Something went wrong!",model)
# 				return(FALSE)
# 			}
# 		)

# 	if(!found_MLE){
# 		quit("no",status = -1)
# 	}
# 	else{
		
# 	}
# 	return(found_MLE)
# }

needed_params <- function(model){
	# TODO:
	# 		should be able to make of log of missing function instead of just
	# 		breaking when function is not found
	# 		ex: 'as.logical' way using 'findFunction' to log it.
	cat("needed_params :")

	found_params_function <- tryCatch(findFunction(paste(model,get(paste(model,"methods",sep="_")),"MLE",sep="_")),
			# ------Future Version to include approach--------------
			# get(paste(model,get(paste(model,"method",sep="_"),get(paste(model,"approach",sep="_")))))
			#-------------------------------------------------------
			warning = function(){
				print(w)
				quit("no", status=-1)
			},
			error = function(e){
				print(e)
				quit("no", status=-1)
			}
		)
	# cat(found_params_function)
	if(is.na(found_params_function)){
		found_params_function <- FALSE
		print("<Error: function not defined>")
		quit("no", status=-1)
	}

	# print(found_params_function)
	else if(!found_params_function){
		cat("<Error: Function not defined> : Please check if function is defined appropriately. \n\t\t\tPlease look at this doc for naming conventions\n")
		quit("no", status = -1)
	}
	cat(paste(" Defined","\n"))
}

needed_MVF <- function(model, obj){
	cat("needed_MVF: ")
	found_MVF <- tryCatch(as.logical(
						findFunction(
							paste(model,"MVF",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(e)
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_MVF){
		quit("no",status = -1)
	}
	else{
		# TODO:
			# test for 'return' of MVF
	}
	cat(paste(found_MVF,"\n"))
	return(found_MVF)
}

needed_MVF_inv <- function(model, obj){
	cat("needed_MVF_inv :")
	found_MVF <- tryCatch(as.logical(
						findFunction(
							paste(model,"MVF_inv",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(e)
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_MVF){
		quit("no",status = -1)
	}
	else{
		# TODO:
			# test for 'return' of MVF
	}
	cat(paste(found_MVF,"\n"))
	return(found_MVF)

}

needed_MVF_cont <- function(model, obj){
	cat("needed_MTTF_cont :")
	found_MVF_cont <- tryCatch(as.logical(
						findFunction(
							paste(model,"MVF_cont",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(e)
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_MVF_cont){
		quit("no",status = -1)
	}
	else{
		# TODO:
			# test for 'return' of MVF
	}
	cat(paste(found_MVF_cont,"\n"))
	return(found_MVF_cont)
}

needed_MTTF <- function(model, obj){
	cat("needed_MTTF :")
	found_MTTF <- tryCatch(as.logical(
						findFunction(
							paste(model,"MTTF",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_MTTF){
		quit("no",status = -1)
	}
	else{

	}
	cat(paste(found_MTTF,"\n"))
	return(found_MTTF)
}

needed_FI <- function(model, obj){
	cat("needed_FI :")
	found_FI <- tryCatch(as.logical(
						findFunction(
							paste(model,"FI",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_FI) quit("no",status = -1)
	cat(paste(found_FI,"\n"))
	return(found_FI)
}

needed_R <- function(model, obj){
	cat("needed_R :")
	found_R <- tryCatch(as.logical(
						findFunction(
							paste(model,"R",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_R) quit("no",status = -1)
	cat(paste(found_R,"\n"))
	return(found_R)
}

needed_lnL <- function(model, obj){
	cat("needed_lnL :")
	found_lnL <- tryCatch(as.logical(
						findFunction(
							paste(model,"lnL",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_lnL) quit("no",status = -1)
	cat(paste(found_lnL,"\n"))
	return(found_lnL)
}

needed_R_growth <- function(model, obj){
	cat("needed_R_growth :")
	found_R_growth <- tryCatch(as.logical(
						findFunction(
							paste(model,"R_growth",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_R_growth)quit("no",status = -1)
	cat(paste(found_R_growth,"\n"))
	return(found_R_growth)
}

needed_FaultsRemaining <- function(model, obj){
	cat("needed_FaultsRemaining:")
	found_FaultsRemaining <- tryCatch(as.logical(
						findFunction(
							paste(model,"FaultsRemaining",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_FaultsRemaining)quit("no",status = -1)
	cat(paste(found_FaultsRemaining,"\n"))
	return(found_FaultsRemaining)
}

needed_R_delta <- function(model, obj){
	cat("needed_R_delta :")
	found_R_delta <- tryCatch(as.logical(
						findFunction(
							paste(model,"R_delta",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(e)
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_R_delta){
		quit("no",status = -1)
	}
	else{
		# TODO:
			# test for 'return' of MVF
	}
	cat(paste(found_R_delta,"\n"))
	

	return(found_R_delta)
}

# We dont care how R_Root is found for now

needed_Target_T <- function(model, obj){
	cat("needed_Target_T :")
	found_Target_T <- tryCatch(as.logical(
						findFunction(
							paste(model,"Target_T",sep="_"))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(e)
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_Target_T){
		quit("no",status = -1)
	}
	else{
		# TODO:
			# test for 'return' of MVF
	}
	cat(paste(found_Target_T,"\n"))
	return(found_Target_T)
}