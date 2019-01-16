# Rentabilidade do Tesouro Direto

Aqui estarei colocando dois modelos de script. Um simplificado e um acurado.

O simplificado considera que os rendimentos mensais possuem a mesma data de depósito que o título mais antigo. 
Em outras palavras, ele desconta o Imposto de Renda (IR) e o IOF de todo o montante acumulado. Isso acaba reduzindo o impacto que os impostos têm sobre o montante.

O modelo acurado cria uma tabela para registrar todos os depósitos, tratando-os individualmente, com suas respectivas datas de depósito e impostos. Também é um modelo mais demorado, e pode levar alguns segundos até que o cálculo termine, para simulações de aportes mensais por mais de 20 anos.

Os modelos foram configurados para aceitar datas de entrada e saída, aportes em dias específicos do mês, presença ou ausência de IR, custódia ou IOF, diferentes taxas de rendimento, etc. Portanto, podem ser usados para comparações acuradas de rendimentos como CDBs, LCAs, Tesouro Direto, Poupança, etc.
