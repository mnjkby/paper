# All credits go to authors of SPEI R package
# https://cran.r-project.org/web/packages/SPEI/index.html

SPEI_daily <- function(input, scale = 21){

  watter_balance <- input[, "WB"]
  watter_balance = zooreg(watter_balance, start=as.Date(min(input$date)))
  watter_balance = as.ts(watter_balance)

  kernel=list(type='rectangular',shift=0)
  distribution='log-Logistic'
  fit='ub-pwm'
  na.rm=FALSE
  ref.start=NULL
  ref.end=NULL
  x=FALSE
  params=NULL

  scale <- as.numeric(scale)
  na.rm <- as.logical(na.rm)
  x <- as.logical(x)
  
  if (!exists("watter_balance",inherits=F) | !exists("scale",inherits=F)) {
  	stop('Both watter_balance and scale must be provided')
  }
  
  if (!(distribution %in% c('log-Logistic', 'Gamma', 'PearsonIII'))) {
    stop('Distrib must be one of "log-Logistic", "Gamma" or "PearsonIII"')
  }
  if (!(fit %in% c('max-lik', 'ub-pwm', 'pp-pwm'))) {
    stop('Method must be one of "ub-pwm" (default), "pp-pwm" or "max-lik"')
  }
  if ( (!is.null(ref.start) && length(ref.start)!=2) | (!is.null(ref.end) && length(ref.end)!=2) ) {
    stop('Start and end of the reference period must be a numeric vector of length two.')
  }

  if (!is.ts(watter_balance)) {
    watter_balance <- ts(as.matrix(watter_balance), frequency = 12, start = c(1950, 1))
  } else {
    watter_balance <- ts(as.matrix(watter_balance), frequency=frequency(watter_balance), start=start(watter_balance))
  }
  m <- ncol(watter_balance)
  fr <- frequency(watter_balance)


  coef = switch(distribution,
                "Gamma" = array(NA,c(2,m,fr),list(par=c('alpha','beta'),colnames(watter_balance),NULL)),
                "log-Logistic" = array(NA,c(3,m,fr),list(par=c('xi','alpha','kappa'),colnames(watter_balance),NULL)),
                "PearsonIII" = coef <- array(NA,c(3,m,fr),list(par=c('mu','sigma','gamma'),colnames(watter_balance),NULL))
  )

  dim_one = ifelse(distribution == "Gamma", 2, 3)

  if (!is.null(params)) {
    if (dim(params)[1]!=dim_one | dim(params)[2]!=m | dim(params)[3]!=12) {
      stop(paste('parameters array should have dimensions (', dim_one, ', ', m, ', 12)',sep=' '))
    }
  }

  # Loop through series (columns in watter_balance)
  if (!is.null(ref.start) && !is.null(ref.end)) {
    watter_balance.fit <- window(watter_balance,ref.start,ref.end)
  } else {
    watter_balance.fit <- watter_balance
  }
  std <- watter_balance*NA
  for (s in 1:m) {
    # Cumulative series (acu)
    acu <- watter_balance.fit[,s]
    acu.pred <- watter_balance[,s]
    if (scale>1) {
      wgt <- kern(scale,kernel$type,kernel$shift)
      acu[scale:length(acu)] <- rowSums(embed(acu,scale)*wgt,na.rm=na.rm)
      acu[1:(scale-1)] <- NA
      acu.pred[scale:length(acu.pred)] <- rowSums(embed(acu.pred,scale)*wgt,na.rm=na.rm)
      acu.pred[1:(scale-1)] <- NA
    }

    # Loop through the months
    for (c in (1:fr)) {
      # Filter month m, excluding NAs
      f <- which(cycle(acu)==c)
      f <- f[!is.na(acu[f])]
      ff <- which(cycle(acu.pred)==c)
      ff <- ff[!is.na(acu.pred[ff])]

      # Monthly series, sorted
      month <- sort.default(acu[f], method="quick")

      if (length(month)==0) {
        std[f] <- NA
        next()
      }

      if (is.null(params)) {
        month_sd = sd(month,na.rm=TRUE)
        if (is.na(month_sd) || (month_sd == 0)) {
          std[f] <- NA
          next
        }

        if(distribution != "log-Logistic"){
          pze <- sum(month==0)/length(month)
          month = month[month > 0]
        }

        # Stop early and assign NAs if month's watter_balance is length < 4
        if(length(month) < 4){
          std[ff,s] = NA
          coef[,s,c] <- NA
          next
        }

        # Calculate probability weighted moments based on fit with lmomco or TLMoments
        pwm = switch(fit,
                     "pp-pwm" = pwm.pp(month,-0.35,0, nmom=3),
                     #pwm.ub(month, nmom=3)
                     TLMoments::PWM(month, order=0:2)
        )

        # Check L-moments validity
        lmom <- pwm2lmom(pwm)
        if ( !are.lmom.valid(lmom) || anyNA(lmom[[1]]) || any(is.nan(lmom[[1]])) ){
          next
        }

        # lmom fortran functions need specific inputs L1, L2, T3
        # this is handled by lmomco internally with lmorph
        fortran_vec = c(lmom$lambdas[1:2], lmom$ratios[3])

        # Calculate parameters based on distribution with lmom then lmomco
        f_params = switch(distribution,
                          "log-Logistic" = tryCatch(lmom::pelglo(fortran_vec), error = function(e){ parglo(lmom)$para }),
                          "Gamma" = tryCatch(lmom::pelgam(fortran_vec), error = function(e){ pargam(lmom)$para }),
                          "PearsonIII" = tryCatch(lmom::pelpe3(fortran_vec), error = function(e){ parpe3(lmom)$para })
        )

        # Adjust if user chose log-Logistic and max-lik
        if(distribution == 'log-Logistic' && fit=='max-lik'){
          f_params = parglo.maxlik(month, f_params)$para
        }
      } else {

        f_params = as.vector(params[,s,c])

      }

      # Calculate cdf based on distribution with lmom
      cdf_res = switch(distribution,
                       "log-Logistic" = lmom::cdfglo(acu.pred[ff], f_params),
                       "Gamma" = lmom::cdfgam(acu.pred[ff], f_params),
                       "PearsonIII" = lmom::cdfpe3(acu.pred[ff], f_params)
      )

      std[ff,s] = qnorm(cdf_res)
      coef[,s,c] <- f_params

      # Adjust if user chose Gamma or PearsonIII
      if(distribution != 'log-Logistic'){
        std[ff,s] = qnorm(pze + (1-pze)*pnorm(std[ff,s]))
      }

    } # next c (month)
  } # next s (series)
  colnames(std) <- colnames(watter_balance)

  z <- list(call=match.call(expand.dots=FALSE),
            fitted=std,coefficients=coef,scale=scale,kernel=list(type=kernel$type,
                                                                 shift=kernel$shift,values=kern(scale,kernel$type,kernel$shift)),
            distribution=distribution,fit=fit,na.action=na.rm)
  if (x) z$watter_balance <- watter_balance
  if (!is.null(ref.start)) z$ref.period <- rbind(ref.start,ref.end)

  input$SPEI <- as.numeric(z$fitted)
  colnames(input)[ncol(input)] <- paste0("SPEI_",scale)

  input
}
