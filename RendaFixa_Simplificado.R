# Ctrl + Shift + S PREVENTS SHOWING EACH COMMAND IN THE OUTPUT


# Funções
Day <- function(x)
{
  format(as.Date(x, format="%Y-%m-%d"), "%d")
}

Month <- function(x)
{
  format(as.Date(x, format="%Y-%m-%d"), "%m")
}

Year <- function(x)
{
  format(as.Date(x, format="%Y-%m-%d"), "%y")
}
Rendimento_M <- function(x)
{
  (1 + x)^(12/365) - 1
}
Rendimento_A <- function(x)
{
  (1 + x)^(1/365) - 1
}
Cust_Anual <- function(x)
{
  (1 + x)^(1/2) - 1
}

# Variáveis estabelecidas pelo usuário
inicial <- as.Date("15-01-19", format="%d-%m-%y")
final   <- as.Date("01-03-23", format="%d-%m-%y")

Rendimento_D <- Rendimento_A(0.065) # 6,5% anual
Cust_Semestral <- Cust_Anual(0.0025) # 0,25% anual

Montante_Inicial <- 1000 # Aporte inicial

Deposito_Mensal <- 100
Dia_do_Deposito <- 15

Mes_Custodia <- c("01","07") # Janeiro e Julho

# Cálculo do rendimento bruto, incluindo custódias automáticas
1 -> i # dias
0 -> j # armazenagem temporária para as custódias semestrais
0 -> Ultima_Custodia
0 -> Lucro
0 -> Custodia_Total
Tabela_Rendimentos <- data.frame(cbind(Data = c(0), Inicial = c(0), Valor = c(0), Lucro = c(0), Custodia = c(0)))
Montante <- Montante_Inicial

while(inicial <= final)
  {

  # O primeiro dia não rende
  if(i == 1) 
  {
    i <- i + 1
    inicial <- inicial + 1
    next 
  }

  Lucro <- Lucro + Montante*Rendimento_D
  Montante <- Montante + Montante*Rendimento_D
  
  # Checar se é dia e mês de custódia, descontar se > 10
  if(Day(inicial) == "01")
  {
    if(Month(inicial) %in% Mes_Custodia)
    {
      j <- j + Montante*Cust_Semestral
      Ultima_Custodia <- inicial

      if(j >= 10)
      {

        Lucro <- Lucro - j
        Montante <- Montante - j
        Custodia_Total <- Custodia_Total + j
        j <- 0
      }
    }
  }
  
  # Checar se é dia de depósito, para adição mensal
  if(Day(inicial) == "01")
  {
    Montante <- Montante + Deposito_Mensal
  }

  # Se for o último dia, cobra o restante da custódia, mais o proporcional até a data, e finaliza
  if(inicial == final)
  {
    Lucro <- Lucro - j - Montante*(1.0025^(as.numeric(final-Ultima_Custodia)/365)-1)
    Montante <- Montante - j - Montante*(1.0025^(as.numeric(final-Ultima_Custodia)/365)-1)
    Custodia_Total <- Custodia_Total + j + Montante*(1.0025^(as.numeric(final-Ultima_Custodia)/365)-1)
    break
  }
  
  inicial <- inicial + 1
  i = i + 1
  next
}
  
# Cobrar IOF, caso o número de dias seja inferior a 30
# Dia do depósito i = 1 (i = 2 = um dia de rendimento)
IOF_tabela <- paste0(rep(seq(0.90, 0, -0.1), each=3) + rep(c(0.06,0.03,0), 10))

0 -> IOF

if((i - 1) <= 30){
  IOF <- Lucro * IOF_tabela[i - 1]
}
Lucro <- Lucro - IOF
Montante <- Montante - IOF

# Cobrar IR após a cobrança do IOF
# Função
IR <- function(dias)
  {
  if(dias <181)
  {
    0.225
  }
  else
    if(dias <361)
    {
      0.20
    }
  else
    if(dias<721)
    {
      0.175
    }
  else
    0.15
}

# Cálculo do IR devido
Imposto_de_Renda <- Lucro*IR(i - 1)
Lucro <- Lucro - Imposto_de_Renda
Montante <- Montante - Imposto_de_Renda

# Extrato Final

print(paste0("Dias de rendimento: ", i - 1))
print(paste0("Depositado: ", Montante - Lucro))
print(paste0("Montante: ", Montante))
print(paste0("Lucro: ", Lucro))
print(paste0("Custódia Total: ", Custodia_Total))
print(paste0("IOF: ", IOF))
print(paste0("Imposto de Renda: ", Imposto_de_Renda))
print(paste0("Rendimento anual médio: ", (Lucro/(Montante-Lucro) + 1)^(365/(i-1))-1   ))
