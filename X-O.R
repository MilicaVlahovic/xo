#############
##  X - O  ##
#############

crtanje_table <- function(trenutna.tabla) {
  xo = c("X", " ", "O") # Simboli
  par(mar = rep(1,4))
  plot.new()
  plot.window(xlim = c(0,30), ylim = c(0,30))
  abline(h = c(10, 20), col = "darkgrey", lwd = 4)
  abline(v = c(10, 20), col = "darkgrey", lwd = 4)
  text(rep(c(5, 15, 25), 3), c(rep(25, 3), rep(15,3), rep(5, 3)), 
       xo[trenutna.tabla + 2], cex = 4)
  # Prepoznavanje lokacije bilo koje 3 u redu
  square = matrix(trenutna.tabla, nrow = 3, byrow = TRUE)
  hor = abs(rowSums(square))
  if (any(hor == 3)) 
    hor = (4 - which(hor == 3)) * 10 - 5 
  else 
    hor = 0
  ver = abs(colSums(square))
  if (any(ver == 3)) 
    ver = which(ver == 3) * 10 - 5 
  else
    ver = 0
  diag1 = sum(diag(square))
  diag2 = sum(diag(t(apply(square, 2, rev)))) 
  # Crtanje pobednickih linija
  if (all(hor > 0))
    for (i in hor)
      lines(c(0, 30), rep(i, 2), lwd = 10, col="red")
  if (all(ver > 0))
    for (i in ver)
      lines(rep(i, 2), c(0, 30), lwd = 10, col="red")
  if (abs(diag1) == 3)
    lines(c(2, 28), c(28, 2), lwd = 10, col = "red")
  if (abs(diag2) == 3)
    lines(c(2, 28), c(2, 28), lwd = 10, col = "red")
}

potez.coveka <- function(trenutna.tabla) {
  text(4, 0, "Klikni na zeljeno polje!", col = "grey", cex=.7)
  prazna.polja  = which(trenutna.tabla == 0)
  potez         = 0
  while (!potez %in% prazna.polja) {
    koordinate    = locator(n = 1) # dodaj liniju
    koordinate$x  = floor(abs(koordinate$x) / 10) + 1
    koordinate$y  = floor(abs(koordinate$y) / 10) + 1
    potez         = koordinate$x + 3 * (3 - koordinate$y)
  }
  return(potez)  # vraca poziciju kvadrata na koji smo kliknuli u obliku broja (1-9)
}

pobednik <- function(trenutna.tabla) {
  igra  = matrix(trenutna.tabla, nrow = 3, byrow = T)
  hor   = rowSums(igra)
  ver   = colSums(igra)
  diag  = c(sum(diag(igra)), sum(diag(apply(igra, 1, rev))))
  if (-3 %in% c(hor, ver, diag))  # 3*(-1) u liniji, pa je pobedio racunar
    return(-10)
  if (3 %in% c(hor, ver, diag))   # 3*(1) u liniji, pa je pobedio covek
    return(10)
  else
    return(0)
}

MK <- function(trenutna.tabla, N){
  vektor = rep(0,9)  # vektor u kome upisujemo rezultate za sva polja za N igara
  zauzeto = which(trenutna.tabla!=0)
  vektor[zauzeto] = -N
  potezi = which(trenutna.tabla==0)   # pozicije praznih polja - moguci potezi
  rezultat = 0
  if(length(potezi) == 1) {  # ako je slobodna samo jedna pozicija, vrati nam nju
    return(potezi)
  }
  
  for (i in potezi){
    tabla=trenutna.tabla
    kpotezi=potezi[potezi!=i]
    tabla[i]=-1
    tabla2=tabla
    rezultat=pobednik(tabla)
    if(rezultat!=0){
      if (rezultat==-10){
        vektor[i]=N
      }
    }
    else{
      for(k in 1:N){
        kpotezi=potezi[potezi!=i]
        while (0 %in% tabla & rezultat == 0) {
          potez=sample(kpotezi, 1)
          if(sum(tabla)==0){
            tabla[potez]=1
          }
          else{
            tabla[potez]=-1
          }
          rezultat=pobednik(tabla)
          kpotezi=kpotezi[kpotezi!=potez]
        }
        if(rezultat==-10){
          vektor[i]=vektor[i]+1
        }
        if(rezultat==10){
          vektor[i]=vektor[i]-1
        }
        rezultat=0
        tabla=tabla2
      
      }
    }
  }
  
  return(which.max(vektor))

}


iks.oks <- function(igrac1 = "covek", igrac2 = "racunar") {
  trenutna.tabla  = rep(0, 9)                       # Prazna tabla
  rezultat        = 0                               # postavljamo rezultat na 0
  igrac           = 1                               # Prvi igrac
  igraci          = c(igrac1, igrac2)
  crtanje_table(trenutna.tabla)
  while (0 %in% trenutna.tabla & rezultat == 0) {   # Nastavi sa igrom dok se tabla ne popuni ili dok se ne nadje pobednik
    if (igraci[(igrac + 3) %% 3] == "covek")        # covek na potezu
      potez = potez.coveka(trenutna.tabla)
    else {                                          # racunar na potezu
      potez = MK(trenutna.tabla,100)          # Monte-Karlo simulacijom dolazimo do najboljeg poteza
    }
    trenutna.tabla[potez] = igrac                   # Promena trenutne table
    crtanje_table(trenutna.tabla)
    rezultat = pobednik(trenutna.tabla)
    igrac = -igrac                                  # Promena igraca
  }
  if (rezultat == 0)
    text(15, 15 , "NERESENO", col = "red", cex = 5)
}

iks.oks()
