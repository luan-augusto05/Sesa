# pacotes
library(tidyverse)
library(lubridate)

# dados processados sobre absenteismo
df_absenteismo = read.csv('absenteismo_processed.csv')
# dados processados sobre alertas no whatsapp
df_whatsapp = read.csv('whatsapp_processed.csv')

glimpse(df_absenteismo)
glimpse(df_whatsapp)

# calcular os totais e a taxa de absenteísmo
df_totais = df_absenteismo |> 
  filter(hospital_padronizado != 'HOSPITAL DE MESSEJANA') |> 
  group_by(periodo) |> 
  summarise(Faltas = sum(faltas, na.rm = TRUE),
            Agendamentos = sum(agendamentos, na.rm = TRUE),
            TaxaAbsenteismo = (sum(Faltas) / sum(Agendamentos)) * 100,
            .groups = 'drop')

# calcular o fator de escala para a linha
max_quantidade = max(df_totais$Faltas, df_totais$Agendamentos, na.rm = TRUE)
fator_escala = max_quantidade / 100

# 📈 contruindo o grafico com os tres indicadores
df_totais |> 
  pivot_longer(
    cols = c(Faltas, Agendamentos),
    names_to = 'Tipo',
    values_to = 'Quantidade') |> 
  ggplot(aes(x = periodo)) +
  geom_bar(aes(y = Quantidade, fill = Tipo),
           stat = 'identity', position = 'dodge', alpha = 0.8) +
  geom_text(aes(y = Quantidade/2, label = ifelse(Tipo == 'Faltas', Quantidade, ''), 
                group = Tipo),position = position_dodge(width = 0.9),
            color = 'white', size = 3.5, fontface = 'bold',
            show.legend = FALSE) +
  geom_text(aes(y = Quantidade, label = ifelse(Tipo == 'Agendamentos', Quantidade, ''), group = Tipo),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5, fontface = 'bold', color = '#3BA9DB',
            show.legend = FALSE) +
  
  # Linha para a taxa de absenteísmo
  geom_line(aes(y = TaxaAbsenteismo * fator_escala, group = 1), 
            color = '#2c5282', size = 1.5, alpha = 0.8) +
  geom_point(aes(y = TaxaAbsenteismo * fator_escala), 
             color = '#2c5282', size = 3) +
  geom_text(aes(y = TaxaAbsenteismo * fator_escala, 
                label = paste0(round(TaxaAbsenteismo, 1), '%')),
            vjust = -1, size = 3.5, color = '#2c5282', fontface = 'bold') +
  scale_y_continuous(
    name = '',
    sec.axis = sec_axis(~ . / fator_escala, 
                        name = '',
                        labels = function(x) paste0(round(x, 2), '%'))
  ) +
  labs(title = '', x = '', fill = '') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = 'black'),
    axis.text.y.left = element_text(color = 'black'),
    axis.title.y.right = element_text(color = 'black'),
    axis.text.y.right = element_text(color = 'black'),
    panel.grid.major.y = element_line(color = 'gray80'),
    panel.grid.minor.y = element_blank(),
    legend.position = 'top'
  ) +
  scale_fill_manual(values = c('Faltas' = '#1C4F66', 'Agendamentos' = '#3BA9DB'))


# 📈 gerando o grafico com os tres indicadores para cada hospital
hospitais = unique(df_absenteismo$hospital_padronizado)
hospitais = hospitais[hospitais != 'HOSPITAL DE MESSEJANA']

# funcao para criar o grafico para cada hospital
criar_grafico_hospital = function(hospital_nome) {
  
  # filtrar dados para o hospital especifico
  df_hospital = df_absenteismo |> 
    filter(hospital_padronizado == hospital_nome) |> 
    group_by(periodo) |> 
    summarise(
      Faltas = sum(faltas, na.rm = TRUE),
      Agendamentos = sum(agendamentos, na.rm = TRUE),
      TaxaAbsenteismo = ifelse(Agendamentos > 0, (Faltas / Agendamentos) * 100, 0),
      .groups = 'drop'
    )
  
  # calcular fator de escala para este hospital
  max_quantidade = max(df_hospital$Faltas, df_hospital$Agendamentos, na.rm = TRUE)
  if (max_quantidade == 0 || is.infinite(max_quantidade)) {
    fator_escala = 1
  } else {
    fator_escala = max_quantidade / 100
  }
  
  # criar o gráfico
  p = df_hospital |> 
    pivot_longer(
      cols = c(Faltas, Agendamentos),
      names_to = 'Tipo',
      values_to = 'Quantidade') |> 
    ggplot(aes(x = periodo)) +
    geom_bar(aes(y = Quantidade, fill = Tipo),
             stat = 'identity', position = 'dodge', alpha = 0.8) +
    geom_text(aes(y = Quantidade/2, label = ifelse(Tipo == 'Faltas', Quantidade, ''), 
                  group = Tipo),
              position = position_dodge(width = 0.9),
              color = 'white', size = 3, fontface = 'bold',
              show.legend = FALSE) +
    geom_text(aes(y = Quantidade, label = ifelse(Tipo == 'Agendamentos', Quantidade, ''), 
                  group = Tipo),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3, fontface = 'bold', color = '#3BA9DB',
              show.legend = FALSE) +
    geom_line(aes(y = TaxaAbsenteismo * fator_escala, group = 1), 
              color = '#2c5282', size = 1.5, alpha = 0.8) +
    geom_point(aes(y = TaxaAbsenteismo * fator_escala), 
               color = '#2c5282', size = 3) +
    geom_text(aes(y = TaxaAbsenteismo * fator_escala, 
                  label = paste0(round(TaxaAbsenteismo, 1), '%')),
              vjust = -1, size = 3, color = '#2c5282', fontface = 'bold') +
    scale_y_continuous(
      name = '',
      sec.axis = sec_axis(~ . / fator_escala, 
                          name = '',
                          labels = function(x) paste0(round(x, 1), '%'))
    ) +
    labs(
      title = hospital_nome,
      x = '', 
      fill = ''
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.title.y.left = element_text(color = 'black'),
      axis.text.y.left = element_text(color = 'black'),
      axis.title.y.right = element_text(color = 'black'),
      axis.text.y.right = element_text(color = 'black'),
      panel.grid.major.y = element_line(color = 'gray80'),
      panel.grid.minor.y = element_blank(),
      legend.position = 'top',
      plot.title = element_text(face = 'bold', hjust = 0.5, size = 12)
    ) +
    scale_fill_manual(values = c('Faltas' = '#1C4F66', 'Agendamentos' = '#3BA9DB'))
  
  return(p)
}

# criar graficos para cada hospital
for (hospital in hospitais) {
  print(criar_grafico_hospital(hospital))
}

# 📊 medidas descritivas sobre a taxa de absenteismo para cada hospital
df_absenteismo |> 
  filter(hospital_padronizado != 'HOSPITAL DE MESSEJANA') |> 
  group_by(hospital_padronizado) |> 
  summarise(taxa_media = mean(taxa_absenteismo, na.rm = TRUE),
            taxa_mediana = median(taxa_absenteismo, na.rm = TRUE),
            dp_taxa = sd(taxa_absenteismo, na.rm = TRUE),
            cv_taxa = sd(taxa_absenteismo, na.rm = TRUE) / mean(taxa_absenteismo, na.rm = TRUE)) |> 
  mutate(cv_taxa  = scales::percent(cv_taxa, accuracy = 0.01))

# 🔍 na base df_whatsapp estao os registros dos disparos apos a politica publica ser implementada, ou seja, ha registros de marco ate o mes de agosto

# 🔍 na base df_whatsapp ha registros de 69 hospitais, entretanto na base df_absenteismo ha registros de 12 hospitais, portanto para efeito de comparacao entre antes e depois da politica serao considerados os doze hospitais da base df_absenteismo

glimpse(df_whatsapp)

# 📈 visualizando o total de disparos por mes e por dia da semana 

# filtrando os registros que estao em ambas as bases
df_whatsapp_filtrado = df_whatsapp |> 
  inner_join(df_absenteismo |> 
               select(hospital_padronizado) |> 
               distinct(), by = 'hospital_padronizado') |> 
  filter(hospital_padronizado != 'HOSPITAL DE MESSEJANA')

# a analise sera feita acerca desses 11 hospitais
length(table(df_whatsapp_filtrado$hospital_padronizado))

# total de disparos por hospital
df_disparos = df_whatsapp_filtrado |> 
  group_by(hospital_padronizado) |> 
  summarise(disparos = sum(count, na.rm = TRUE)) |> 
  arrange(desc(disparos))


df_disparos |> 
  ggplot(aes(x = reorder(hospital_padronizado, disparos), y = disparos)) +
  geom_bar(stat = 'identity', fill = '#3BA9DB', alpha = 0.8) +
  geom_text(aes(label = disparos), 
            hjust = -0.1, size = 3.5, fontface = 'bold', color = 'black') +
  coord_flip() +
  labs(title = '',
       x = '',
       y = '') +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


# 📈 visualizacao do total de disparos por cidade
df_whatsapp |> 
  count(cidade, tipo_alerta) |> 
  rename(Frequencia = n) |> 
  ggplot(aes(x = tipo_alerta, y = cidade, fill = Frequencia)) +
  geom_tile(color = 'white', linewidth = 0.3) +
  geom_text(aes(label = ifelse(Frequencia > 0, Frequencia, '')), 
            color = 'white', size = 2.5, fontface = 'bold') +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue', 
                      name = '', trans = 'log10') +
  labs(title = '', subtitle = paste('Total de', sum(df_whatsapp$count), 'Disparos'),
    x = '', y = '') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    panel.grid = element_blank())


# 📈 visualizacao do total de disparos por hospital
df_whatsapp_filtrado |> 
  group_by(hospital_padronizado, tipo_alerta) |> 
  summarise(Total_Disparos = sum(count, na.rm = TRUE), .groups = 'drop') |> 
  ggplot(aes(x = tipo_alerta, y = hospital_padronizado, fill = Total_Disparos)) +
  geom_tile(color = 'white', linewidth = 0.3) +
  geom_text(aes(label = ifelse(Total_Disparos > 0, 
                               format(Total_Disparos, big.mark = '.'), 
                               '')), 
            color = 'white', size = 2.5, fontface = 'bold') +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue', name = '', trans = 'log10') +
  labs(title = '', subtitle = paste('Total de', format(sum(df_whatsapp_filtrado$count, na.rm = TRUE), big.mark = '.'), 'Disparos'), x = '', y = '') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    panel.grid = element_blank(),
    legend.position = 'none')


# quantidade de disparos mensais
df_disparos_mensais = df_whatsapp_filtrado |> 
  group_by(year, month, hospital_padronizado) |> 
  summarise(total_disparos = sum(count, na.rm = TRUE), .groups = 'drop') |> 
  arrange(year, month, desc(total_disparos)) |> 
  mutate(mes_ano = paste(year, sprintf('%02d', month), sep = '-')) |>
  select(year, month, mes_ano, everything())






