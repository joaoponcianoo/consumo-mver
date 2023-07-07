# JOB Atualiza Consumo

Nesse desenvolvimento o cliente precisava de um JOB que verificasse o consumo das últimas 12 semanas de seus produtos.

### Regra de negócio:

Select na tabela MARC. Para cada MATNR/BUKRS verificar o consumo na tabela MVER.

Se não houver nenhum consumo nas últimas 12 semanas ou nenhum cadastro de MATNR e BUKRS na tabela MVER, deve ser criado/atualizado a tabela MVER campo MGV__"uma semana antes da atual" com o valor de 0,100 utilizando a função MVER_MAINTAIN_DARK.

### Identificação da semana atual

Para identificar a semana atual utilizei a função DATE_GET_WEEK onde eu entro com SY-DATUM, retornando a semana atual no ano.

![image](https://github.com/joaoponcianoo/consumo-mver/assets/115370264/01affff7-0d79-4adf-b897-b23aaaeaf5f5)

Com o valor da semana atual é possivel identificar o campo na tabela MVER que deve ser atualizado. MGV__ = semana atual -1.

### Demontração

Últimas 12 semanas sem nenhum consumo:
![image](https://github.com/joaoponcianoo/consumo-mver/assets/115370264/2a36afda-0dff-4495-b842-0bb5ee8f21b2)

Atualização com 0,100 na semana atual -1:
![image](https://github.com/joaoponcianoo/consumo-mver/assets/115370264/3e2bce1b-8d1c-46f6-85e7-796f2b707373)







