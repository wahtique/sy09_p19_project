library(MASS)
log.app <- function(Xapp, zapp, intr, epsi) 
{
  
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]

	Xapp <- as.matrix(Xapp)

	if (intr == T)
	{
		Xapp <- cbind(rep(1,n),Xapp) #On ajoute une colonne de 1 au X
		p <- p + 1  #Permet d'ajouter un biais au jeu de données (sinon Combinaison linéaire)
	}       #Cela ajoute une prédicteur (paramètre supplémentaire)

	targ <- matrix(as.numeric(zapp),nrow=n) 
	targ[which(targ==2),] <- 0    #Codage en 0 et 1 de zapp
	tXap <- t(Xapp)

	beta <- matrix(0,nrow=p,ncol=1)  #Paramètres initiaux

	conv <- F
	iter <- 0
	while (conv == F)  #Convergence
	{
		iter <- iter + 1
		bold <- beta

		prob <- as.vector(postprob(beta, Xapp))
		prob0<-as.vector(postprob(beta ,Xapp))
		MatW <- diag(prob0*(1-prob0)) 

		beta <- beta + MASS::ginv(tXap%*%MatW%*%Xapp)%*%tXap%*%(targ - prob)

		if (norm(beta-bold)<epsi)  #On s'arrete quand la différence entre le vecteur et celui précédent
		{                          #est inférieur à un espilson choisi
			conv <- T
		}
	}

	prob <- postprob(beta, Xapp)
	out <- NULL
	out$beta <- beta
	out$iter <- iter
	out$logL <- sum(targ*log(prob)+(1-targ)*log(1-prob))

	out
}

log.val <- function(beta, Xtst)
{
	m <- dim(Xtst)[1]
	p <- dim(beta)[1]
	pX <- dim(Xtst)[2]

	Xtst <- as.matrix(Xtst)

	if (pX == (p-1)) #On regarde si la taille de beta correspond à celle de Xtst
	{
		Xtst  <- cbind(rep(1,m),Xtst) #On ajoute les intercept si nécéssaire
	}

	prob <- postprob(beta, Xtst)
	pred <- max.col(prob)

	out <- NULL
	out$prob <- prob
	out$pred <- pred

	return(out)
}

postprob <- function(beta, X) #proba à posteriori à la classe 1
{
	X <- as.matrix(X)

	prob <- exp(X%*%beta)/(1+exp(X%*%beta)) #car g = 2
}
