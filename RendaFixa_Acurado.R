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

Add_Row <- function(x)
{
  x <- rbind(x, Tabela_Rendimentos[1,])
}

# Variáveis estabelecidas pelo usuário
inicial <- as.Date("29-01-19", format="%d-%m-%y")
final   <- as.Date("02-02-29", format="%d-%m-%y")

Montante_Inicial <- 1000 # Aporte inicial
Rendimento_D <- Rendimento_A(0.065) # 6,5% anual

Tem_Custodia <- TRUE
Cust_Semestral <- Cust_Anual(0.0025) # 0,25% anual
Mes_Custodia <- c("01","07") # Janeiro e Julho

Tem_Deposito_Mensal <- TRUE
Deposito_Mensal <- 100
Dia_do_Deposito <- 1

Tem_IOF <- TRUE
Tem_IR <- TRUE

# Cálculo do rendimento bruto, incluindo custódias automáticas
1 -> i # dias
inicial -> Ultima_Custodia
0 -> Custodia_Total
Tabela_Rendimentos <- data.frame(Data = as.Date("01-01-01", format="%d-%m-%y"), Inicial = c(0), Valor = c(0), Lucro = c(0), Custodia = c(0))
Montante <- Montante_Inicial
FALSE -> error # Mensagem de erro caso o último depósito fosse feito no dia do resgate

while(inicial <= final)
{
  
  # O primeiro dia não rende
  if(i == 1) 
  {
    Tabela_Rendimentos$Data[i] <- inicial
    Tabela_Rendimentos$Inicial[i] <- Montante_Inicial
    Tabela_Rendimentos$Valor[i] <- Montante_Inicial
    Tabela_Rendimentos$Lucro[i] <- 0
    Tabela_Rendimentos$Custodia[i] <- 0
    i <- i + 1
    inicial <- inicial + 1
    next 
  }
  
  # Rendimento diário
   for(k in 1:nrow(Tabela_Rendimentos))
   {
     Tabela_Rendimentos$Lucro[k] <- Tabela_Rendimentos$Lucro[k] + Tabela_Rendimentos$Valor[k]*Rendimento_D
     Tabela_Rendimentos$Valor[k] <- Tabela_Rendimentos$Valor[k] + Tabela_Rendimentos$Valor[k]*Rendimento_D
   }

  # Checar se é dia e mês de custódia, descontar se > 10
  if(Tem_Custodia){
  if(Day(inicial) == "01")
  {
    if(Month(inicial) %in% Mes_Custodia)
    {
      for(k in 1:nrow(Tabela_Rendimentos))
      {
        Tabela_Rendimentos$Custodia[k] <- Tabela_Rendimentos$Custodia[k] + Tabela_Rendimentos$Valor[k]*Cust_Semestral
      }
      
      # Guarda a data da última custódia
      Ultima_Custodia <- inicial
      
      if(sum(Tabela_Rendimentos$Custodia) >= 10)
      {
        Custodia_Total <- Custodia_Total + sum(Tabela_Rendimentos$Custodia)
        for(k in 1:nrow(Tabela_Rendimentos))
          {
          Tabela_Rendimentos$Lucro[k] <- Tabela_Rendimentos$Lucro[k] - Tabela_Rendimentos$Custodia[k]
          Tabela_Rendimentos$Valor[k] <- Tabela_Rendimentos$Valor[k] - Tabela_Rendimentos$Custodia[k]
          Tabela_Rendimentos$Custodia[k] <- 0
        }
      }
    }
  }
  }
  
  # Checar se é dia de depósito, para adição mensal
  if(Tem_Deposito_Mensal){
  if(Day(inicial) == "01")
    {
    if(inicial == final)
      {
      error <- TRUE
      break
    }
    Tabela_Rendimentos <- Add_Row(Tabela_Rendimentos)
    Tabela_Rendimentos$Data[nrow(Tabela_Rendimentos)] <- inicial
    Tabela_Rendimentos$Inicial[nrow(Tabela_Rendimentos)] <- Deposito_Mensal
    Tabela_Rendimentos$Valor[nrow(Tabela_Rendimentos)] <- Deposito_Mensal
    Tabela_Rendimentos$Lucro[nrow(Tabela_Rendimentos)] <- 0
    Tabela_Rendimentos$Custodia[nrow(Tabela_Rendimentos)] <- 0
  }
  }
  
  # Se for o último dia, cobra o restante da custódia, mais o proporcional até a data, e finaliza
  if(inicial == final)
  {
    Custodia_Total <- Custodia_Total + sum(Tabela_Rendimentos$Custodia) + sum(Tabela_Rendimentos$Valor)*(1.0025^(as.numeric(final-Ultima_Custodia)/365)-1)
    for(k in 1:nrow(Tabela_Rendimentos))
      {
      Tabela_Rendimentos$Lucro[k] <- Tabela_Rendimentos$Lucro[k] - Tabela_Rendimentos$Custodia[k] - Tabela_Rendimentos$Valor[k]*(1.0025^(as.numeric(final-Ultima_Custodia)/365)-1)
      Tabela_Rendimentos$Valor[k] <- Tabela_Rendimentos$Valor[k] - Tabela_Rendimentos$Custodia[k] - Tabela_Rendimentos$Valor[k]*(1.0025^(as.numeric(final-Ultima_Custodia)/365)-1)
      Tabela_Rendimentos$Custodia[k] <- 0
      break
    }
  }
  inicial <- inicial + 1
  #i = i + 1
  next
}

0 -> IOF
# Cobrar IOF, caso o número de dias seja inferior a 30
if(Tem_IOF)
{
  IOF_tabela <- as.numeric(paste0(rep(seq(0.90, 0, -0.1), each=3) + rep(c(0.06,0.03,0), 10)))
  
  for(k in 1:nrow(Tabela_Rendimentos))
  {
    if(final - Tabela_Rendimentos$Data[k] <= 30)
    {
      # IOF_linha criado para armazenar temporariamente o IOF a cada iteração
      IOF_linha <- Tabela_Rendimentos$Lucro[k] * IOF_tabela[final - Tabela_Rendimentos$Data[k]]
      IOF <- IOF + IOF_linha
      Tabela_Rendimentos$Lucro[k] <- Tabela_Rendimentos$Lucro[k] - IOF_linha
      Tabela_Rendimentos$Valor[k] <- Tabela_Rendimentos$Valor[k] - IOF_linha
    }
  }
}


# Cobrar IR nos diferentes montantes, segundo seus períodos se rendimento
# Cálculo do IR devido
IR_Total <- 0
if(Tem_IR)
  {
  b <- as.numeric(final - Tabela_Rendimentos$Data)
  IR_225 <- sum(Tabela_Rendimentos$Lucro[b < 181])
  IR_20 <- sum(Tabela_Rendimentos$Lucro[b < 361 & b >= 181])
  IR_175 <- sum(Tabela_Rendimentos$Lucro[b < 721 & b >= 361])
  IR_15 <- sum(Tabela_Rendimentos$Lucro[b >= 721])
  IR_Total <- IR_225*0.225 + IR_20*0.2 + IR_175*0.175 + IR_15*0.15
} else {
  b <- 0
  IR_225 <- 0
  IR_20 <- 0
  IR_175 <- 0
  IR_15 <- 0
}

# Extrato Final
print(paste0("Dias de rendimento: ", final - Tabela_Rendimentos$Data[1]))
print(paste0("Montante Líquido: ", sum(Tabela_Rendimentos$Valor) - IR_Total))
print(paste0("Depositado: ", sum(Tabela_Rendimentos$Valor) - sum(Tabela_Rendimentos$Lucro)))
if(error == T){print(paste0("O último depósito de ", Deposito_Mensal, " reais não foi computado, por ocorrer no dia do resgate."))}
print(paste0("Lucro Líquido: ", sum(Tabela_Rendimentos$Lucro) - IR_Total))
print(paste0("Custódia Total: ", Custodia_Total))
print(paste0("IOF: ", IOF))
print(paste0("Imposto de Renda: ", IR_Total))
print(paste0("Rendimento (liq) anual médio total: ", (((sum(Tabela_Rendimentos$Lucro) - IR_Total)/(sum(Tabela_Rendimentos$Valor) - sum(Tabela_Rendimentos$Lucro)))+1)^(365/as.numeric(final - Tabela_Rendimentos$Data[1])) - 1 ))
print(paste0("Rendimento (liq) anual médio do título mais antigo: ", (Tabela_Rendimentos$Lucro[1]*0.85/(Tabela_Rendimentos$Valor[1] - Tabela_Rendimentos$Lucro[1]) + 1)^(365/as.numeric(final - Tabela_Rendimentos$Data[1])) - 1 ))
if(Tem_IR){print(paste0("Valor que pode ser retirado mensalmente: ", 0.85*((1+Rendimento_D)^(365/12)-1)*sum(Tabela_Rendimentos$Valor)))}
if(Tem_IR==F){print(paste0("Valor que pode ser retirado mensalmente: ", ((1+Rendimento_D)^(365/12)-1)*sum(Tabela_Rendimentos$Valor)))}










