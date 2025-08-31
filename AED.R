# pacotes
library(tidyverse)

# dados processados sobre absenteismo
df_absenteismo = read.csv('absenteismo_processed.csv')
# dados processados sobre alertas no whatsapp
df_whatsapp = read.csv('whatsapp_processed.csv')

glimpse(df_whatsapp)
glimpse(df_absenteismo)
length(table(df_absenteismo$hospital_padronizado))
# table(df_alertas$severidade)
# table(df_alertas$hospital)
# table(df_alertas$especialidade)
# table(df_alertas$municipio)
# length(table(df_alertas$municipio))

df_absenteismo |> 
  group_by(periodo) |> 
  summarise(
    Faltas = sum(faltas, na.rm = TRUE),
    Agendamentos = sum(agendamentos, na.rm = TRUE),
    .groups = 'drop') |> 
  pivot_longer(
    cols = c(Faltas, Agendamentos),
    names_to = 'Tipo',
    values_to = 'Quantidade') |> 
  ggplot(aes(x = periodo, y = Quantidade, fill = Tipo)) +
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  geom_text(aes(label = Quantidade, color = Tipo),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3, fontface = 'bold',
            show.legend = F) +  
  labs(title = '', x = '', y = '', fill = '') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = 'top'
  ) +
  scale_fill_manual(values = c('Faltas' = '#1C4F66', 'Agendamentos' = '#3BA9DB')) +
  scale_color_manual(values = c('Faltas' = '#1C4F66', 'Agendamentos' = '#3BA9DB')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


# calcular os totais e a taxa de absenteísmo
df_totais = df_absenteismo |> 
  group_by(periodo) |> 
  summarise(Faltas = sum(faltas, na.rm = TRUE),
            Agendamentos = sum(agendamentos, na.rm = TRUE),
            TaxaAbsenteismo = (sum(Faltas) / sum(Agendamentos)) * 100,
            .groups = 'drop')

# calcular o fator de escala para a linha
max_quantidade = max(df_totais$Faltas, df_totais$Agendamentos, na.rm = TRUE)
fator_escala = max_quantidade / 100

# contruindo o grafico com os tres indicadores
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


# 🔍 na base df_whatsapp estao os registros dos disparos apos a politica publica ser implementada, ou seja, ha registros de marco ate o mes de agosto

# 🔍 na base df_whatsapp ha registros de 69 hospitais, entretanto na base df_absenteismo ha registros de 12 hospitais, portanto para efeito de comparacao entre antes e depois da politica serao considerados os doze hospitais da base df_absenteismo

glimpse(df_whatsapp)

# 📈 visualizando o total de disparos por mes e por dia da semana 













