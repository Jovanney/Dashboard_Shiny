
# Define server logic required to draw a histogram
server <- function(input, output) {
  ################### INPUT ####################
  select_stock <- eventReactive(input$go, {
    stock_name <- input$stock
    twin <- input$true_date
    
    master_df <- read_xlsx('amazonia.xlsx') %>%
      group_by(year, state) %>%
      summarise(number = sum(number, na.rm = T)) %>%
      ungroup()
    
    df_stock <- master_df %>% 
      filter(state == stock_name)
    
    #Recebendo os dados da tela
    ano_inicial <- input$stock_ano_inicial
    ano_final <- input$stock_ano_final
    #Convertendon para inteiro
    ano_inicial <- as.numeric(ano_inicial)
    ano_final <- as.numeric(ano_final)
    
    if(ano_final < ano_inicial){
      temp <- ano_final
      ano_final <- ano_inicial
      ano_inicial <- temp
    }
    
    ano_atual <- ano_inicial + 1
    
    #z <- df_stock %>% 
    #filter(between(Ano, ano_inicial, ano_final))
    
    z <- df_stock %>% 
      filter(year == ano_inicial)
    
    #Função que filtra os dados
    while(ano_atual <= ano_final){
      y <- df_stock %>% 
        filter(year == ano_atual)
      
      z <- bind_rows(z, y)
      ano_atual <- ano_atual + 1
    }
    return(z)
  })
  
  select_stock2 <- eventReactive(input$go, {
    
    stock_name <- input$stock
    twin <- input$true_date
    
    #Recebendo os dados da tela
    ano_inicial <- input$stock_ano_inicial
    ano_final <- input$stock_ano_final
    
    #Convertendon para inteiro
    ano_inicial <- as.numeric(ano_inicial)
    ano_final <- as.numeric(ano_final)
    
    if(ano_final < ano_inicial){
      temp <- ano_final
      ano_final <- ano_inicial
      ano_inicial <- temp
    }
    
    master_df <- read_xlsx('amazonia.xlsx')
    
    df_stock <- master_df %>% 
      filter(state == stock_name) %>% 
      filter(between(year, ano_inicial, ano_final))
    
    return(df_stock)
  })
  
  
  
  
  ################ OUTPUT #####################
  Info_DataTable <- eventReactive(input$go,{
    df <- select_stock()
    
    Min <- min(df["number"])
    Max <- max(df["number"])
    Media <- mean(df$number)
    Mediana <- median(df$number)
    
    #obtem a tabela com frequencia das variaveis
    freq <- table(df["number"]);
    #obtem o nome da variavel que mais se repete
    moda <- names(table(df["number"]))[table(df["number"]) == max(table(df["number"]))]
    Moda <- as.numeric(moda)
    
    variancia <- var(df$number)
    Desvio <- variancia^(1/2)
    
    
    Estado <- input$stock
    
    df_tb <-  data.frame(Estado, Media, Moda, Mediana, Min, Max, Desvio)
    
    df_tb <- as.data.frame(t(df_tb))
    
    
    return(df_tb)
  })
  
  output$info <- renderDT({
    Info_DataTable() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  output$hist <- renderPlot({
    #All the inputs
    df <- select_stock()
    
    num <- df$number %>% na.omit() %>% as.numeric()
    num_min <- min(num)
    num_max <- max(num)
    
    ggplot(df, aes(x = number)) +
      geom_histogram(group = 1, binwidth = 100, color = "black", bins = 10000) + geom_bar(start = "count") + scale_x_continuous(limits = c(num_min, num_max), breaks = seq(0,num_max,100)) +
      theme(axis.text = element_text(angle = 90, vjust = 0))
    
  })
  
  output$sh <- renderPlot({
    df <- select_stock()
    
    ggplot(df, aes(x= as.factor(year), y=as.factor(number), group = state, color = state)) +
      geom_line(size = 2) + xlab("Ano") + ylab("Queimadas")
    
  })
  
  output$box <- renderPlot({
    df <- select_stock2()
    
    ggplot(df, aes(x=as.factor(year), y=number)) + 
      geom_boxplot(fill="slateblue", alpha=0.5) + 
      xlab("Ano")
  })
  
  
  mapa1 <- eventReactive(input$go_comp, {
    stock_name1 <- input$Estado1
    
    #Recebendo os dados da tela
    ano_inicial <- input$stock_comp_ano_inicial
    ano_final <- input$stock_comp_ano_final
    #Convertendon para inteiro
    ano_inicial <- as.numeric(ano_inicial)
    ano_final <- as.numeric(ano_final)
    
    if(ano_final < ano_inicial){
      temp <- ano_final
      ano_final <- ano_inicial
      ano_inicial <- temp
    }
    
    master1_df <- read_xlsx('amazonia.xlsx') %>%
      group_by(year, state) %>%
      summarise(number = sum(number, na.rm = T)) %>%
      ungroup()
    
    df1_stock <- master1_df %>% 
      filter(state == stock_name1)
    
    z <- df1_stock %>% 
      filter(between(year, ano_inicial, ano_final))
    
    return(z)
    
  })
  
  mapa2 <- eventReactive(input$go_comp, {
    stock_name2 <- input$Estado2
    
    #Recebendo os dados da tela
    ano_inicial <- input$stock_comp_ano_inicial
    ano_final <- input$stock_comp_ano_final
    #Convertendon para inteiro
    ano_inicial <- as.numeric(ano_inicial)
    ano_final <- as.numeric(ano_final)
    
    if(ano_final < ano_inicial){
      temp <- ano_final
      ano_final <- ano_inicial
      ano_inicial <- temp
    }
    
    master1_df <- read_xlsx('amazonia.xlsx') %>%
      group_by(year, state) %>%
      summarise(number = sum(number, na.rm = T)) %>%
      ungroup()
    
    df2_stock <- master1_df %>% 
      filter(state == stock_name2)
    
    z2 <- df2_stock %>% 
      filter(between(year, ano_inicial, ano_final))
    
    return(z2)
    
  })
  
  
  select_2_stocks <- eventReactive(input$go_comp, {
    
    df <- mapa1()
    df2 <- mapa2()
    
    tabela <- bind_rows(df, df2)
    return(tabela)
    
  })
  
  Info_DataTable2 <- eventReactive(input$go_comp,{
    
    df <- mapa1()
    df2 <- mapa2()
    
    Correlação <- cor(df$number, df2$number)
    Correlação <- data.frame(Correlação)
    Correlação <- as.data.frame(t(Correlação))
    
    return(Correlação)
  })
  
  output$info2 <- renderDT({
    Info_DataTable2() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  output$grafo <- renderPlot({
    df <- select_2_stocks()
    
    ggplot(df, aes(x= as.factor(year), y=as.factor(number), group = state, color = state)) +
      geom_line(size = 2) + xlab("Ano") + ylab("Queimadas")
    
  })
  
  output$barra <- renderPlot({
    df <- select_2_stocks()
    
    ggplot(df, aes(fill = state, y = as.factor(round(number / 12)), x = as.factor(year))) +
      geom_bar(position = "dodge", stat="identity") + ylab("Média de Queimadas") + xlab("Ano")
  })
  
  output$scat <- renderPlot({
    df <- select_2_stocks()
    
    ggplot(df, aes(x = as.factor(year), y = as.factor(number), color = state)) + 
      geom_point(size = 6) + xlab("Ano") + ylab("Queimadas")
  })
}
