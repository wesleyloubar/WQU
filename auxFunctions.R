library(dplyr)
get_colors = function(){ 
colorlist= c("#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58","#6aff66","#f900b3","#adabad","#9c1aaf",
             "#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58","#6aff66","#f900b3",
             "#adabad","#9c1aaf","#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58",
             "#6aff66","#f900b3","#adabad","#9c1aaf","#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f",
             "#6132fc","#ffdb58","#6aff66","#f900b3","#adabad","#9c1aaf","#05aa00","#fff733",
             "#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58","#6aff66","#f900b3","#adabad","#9c1aaf",
             "#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58","#6aff66","#f900b3",
             "#adabad","#9c1aaf","#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58",
             "#6aff66","#f900b3","#adabad","#9c1aaf","#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f",
             "#6132fc","#ffdb58","#6aff66","#f900b3","#adabad","#9c1aaf","#05aa00","#fff733",
             "#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58","#6aff66","#f900b3","#adabad","#9c1aaf",
             "#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58","#6aff66","#f900b3",
             "#adabad","#9c1aaf","#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f","#6132fc","#ffdb58",
             "#6aff66","#f900b3","#adabad","#9c1aaf","#05aa00","#fff733","#44bfa6","#ffffe0","#c675c6","#ff596f",
             "#6132fc","#ffdb58","#6aff66","#f900b3","#adabad","#9c1aaf","#05aa00","#fff733")

nome_cores <- c("Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa",
                "cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul",  "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa",
                "cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul",  "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa",
                "cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul",  "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo","Verde_maritimo", "Amarelo_claro", "Lilas", "Vermelho_leve","Azul", "Mostarda", "Verde iluminado", "Rosa","cinza", "roxo", "Verde_alface", "Amarelo")

codigo_cores <- data.frame(colorlist,nome_cores)
codigo_cores <- unique(codigo_cores)
colnames(codigo_cores) <- c('color_code','color_name')
  return(list(colorlist,nome_cores,codigo_cores))
}

# Function to replace blank space  and hiphen - with underscore _
replace_with_underscore = function(df)
{
  #Rename the unique column to keywords to use it throughout the function
  colnames(df) <- 'keywords'
  # Loop to get each row of keywords
  for (i in (1:nrow(df)))
  {
    # Get the respective keywords line
    line <- as.character(df$keywords[i])
    # Split the ; to get the list of all keywords (the result is a list)
    split_line <- strsplit(line, ";")[1]
    # Convert the list to a dataframe where each line is a single keyword
    df_of_keywords <- as.data.frame(split_line,col.names = ('kwd'))
    
    # The list that will replace the keywords processed
    new_keywords <- c()
    
    # Loop through the line of the keywords dataframe
    for (keyword in df_of_keywords$kwd)
    {
      # Replace " with blank string
      keyword <- gsub("\"", "", keyword)
      # Remove blank space from the beggining and at the end of the string
      keyword <- trimws(keyword, which = "both")
      # Replace blank space with underscore
      keyword <- gsub(" ", "_", keyword)
      # Replace hyphen with underscore
      keyword <- gsub("-", "_", keyword)
      # Add the changed keywords to the new_keywords vector 
      new_keywords <- c(new_keywords,keyword)
    }
    
    # After all keywords have been changed, we get the df with 1 column and many rows and aggregate all the rows
    # into a single row, elements separated by ;
    new_keywords <- paste(new_keywords,collapse=";")
    # Insert the new_keywords into the dataframe
    df$keywords[i] <- as.character(new_keywords)
  }
  # When there is no keywords, the system was adding a character "NA" then I force that string to become a real NA value
  df[df=='NA'] <- NA
  # Return the result
  return (df)
}

process_plurals = function(df)
{
  colnames(df) <- 'kwd'
  # Criar Lista com todas as palavras chaves do dataframe
  # Collapse is used to append the rows of a dataframe superating them by ; . It turns into a huge string
  list_of_all_words <- (paste(df$kwd,collapse=";"))
  
  # I have seen one keyword with two ;; together
  list_of_all_words <- gsub(";;", ";", list_of_all_words)
  # Separate the huge string and turn into a vector where each position is a keyword
  # Searchable character
  list_of_all_words <- as.array(strsplit(list_of_all_words, ";")[1])
  
  #Rename the unique column to keywords to use it throughout the function
  
  
  for (i in (1:nrow(df)))
  {
    # Get the respective keywords line
    line <- as.character(df$kwd[i])
    # Split the ; to get the list of all keywords (the result is a list)
    split_line <- strsplit(line, ";")[1]
    # Convert the list to a dataframe where each line is a single keyword
    df_of_keywords <- as.data.frame(split_line,col.names = ('kwd'))
    
    
    # The list that will replace the keywords processed
    plural_keywords <- c()
    if (!is.na(line))
    { 
      for (keyword in df_of_keywords$kwd)
      {
        # If the keywork end with Y
        
        if(str_sub(keyword,-1,-1) == 'Y')
        {
          # keyword <- 'WESLEY'
          # Delete Y and adds IES (TECHNOLOGY To TECHNOLOGIES)
          aux_keyword <- paste(str_sub(keyword,-(nchar(keyword)),-2),'IES',sep = "")
          # If the simulated plural is in the list of all keywords then saves the plural version of the word
          if(is.element(aux_keyword, list_of_all_words[[1]]))
          {
            # print(keyword)
            # print(aux_keyword)
            plural_keywords <- c(plural_keywords,aux_keyword)
          }else
          {
            # Adds S to a word ending with Y. DAY To DAYS
            aux_keyword <- paste(keyword,'S',sep = "")
            # If the simulated plural is in the list of all keywords then saves the plural version of the word
            if(is.element(aux_keyword, list_of_all_words[[1]]) == TRUE)
            {
              # print(keyword)
              # print(aux_keyword)
              plural_keywords <- c(plural_keywords,aux_keyword)
            }else
            {
              plural_keywords <- c(plural_keywords,keyword)
            }
          }
        } else if(str_sub(keyword,-1,-1) == 'X')
        {
          aux_keyword <- paste(keyword,'ES',sep = "")
          
          if(is.element(aux_keyword, list_of_all_words[[1]]) == TRUE)
          {
            # print(keyword)
            # print(aux_keyword)
            plural_keywords <- c(plural_keywords,aux_keyword)
          }else
          {
            aux_keyword <- paste(str_sub(keyword,-(nchar(keyword)),-2),'CES',sep = "")
            
            if(is.element(aux_keyword, list_of_all_words[[1]]) == TRUE)
            {
              # print(keyword)
              # print(aux_keyword)
              plural_keywords <- c(plural_keywords,aux_keyword)
            }else
              
              plural_keywords <- c(plural_keywords,keyword)
          }
        } else
        {
          aux_keyword <- paste(keyword,'S',sep = "")
          if(is.element(aux_keyword, list_of_all_words[[1]]) == TRUE)
          {
            # print(keyword)
            # print(aux_keyword)
            plural_keywords <- c(plural_keywords,aux_keyword)
          }else
          {
            plural_keywords <- c(plural_keywords,keyword)
          }
        }
        
      }
      plural_keywords <- paste(plural_keywords,collapse=";")
      df$kwd[i] <- as.character(plural_keywords)
    } else
    {
      df$kwd[i] <- NA
    }
  }
  
  return (df)
}

glossario_tc <- function(df, glossario)
{
  print("#########################")
  setwd( "C:/Users/bigda/Documents/Laraia_Wesley/Conectividade_Carros_Eletricos/Arquivos_Cos_Filtrados")
  # glossary <- readRDS(file = "glossary.rds")
  glossary <- glossario
  colnames(glossary) <- c("component" ,   "word_aux"     ,    "action"    ,   "altered_word")
  glossary <- glossary %>% filter(component == 'tc')
  words_to_delete <- glossary %>% select(word_aux) %>% filter(glossary$action == 'delete')
  words_to_delete <- as.character(words_to_delete$word_aux)
  words_to_delete <- toupper(words_to_delete)
  words_to_change <- glossary %>% select(word_aux) %>% filter(glossary$action == 'replace')
  
  words_to_change <- as.character(words_to_change$word_aux)
  words_to_change <- toupper(words_to_change)

  # words_to_delete <- toupper(words_to_delete)
  # words_to_change <- toupper(words_to_change)
  
  # keywords_DE <- as.data.frame(Me$DE,stringsAsFactors = FALSE)
  # colnames(keywords_DE) <- c('keywords_DE')
  # # Get WoS Keywords
  # keywords_ID <- as.data.frame(Me$ID,stringsAsFactors = FALSE)
  # colnames(keywords_ID) <- c('keywords_ID')
  # df <- keywords_DE
  
  
  colnames(df) <- 'kwd'
  # df <- transform(df, kwd = as.character(kwd))
  #Rename the unique column to keywords to use it throughout the function
  for (i in (1:nrow(df)))
  {
    if(length(df$kwd[i])>0)
    {
      line <- trimES(df$kwd[i])
      line <- gsub(' ', '', line, ignore.case = T)

      line_keywords <- tibble(kwd = strsplit(line, ";")[[1]])
      # line_keywords <- as.data.frame(line_keywords)
      antes <- line_keywords$kwd
      line_keywords <- line_keywords %>% filter (!kwd %in% words_to_delete)
      depois <- line_keywords$kwd
 
      for (word in words_to_change)
      {
        keyword <- toupper(trim(word))
        replacement <- glossary[(glossary$action == 'replace'),]
        replacement <- (replacement[(toupper(trim(replacement$word_aux)) == keyword),] )
        new_word <- as.character(replacement$altered_word)
        new_word <- toupper(new_word)
        new_list <- gsub(keyword, new_word, line_keywords$kwd, ignore.case = T)
        line_keywords$kwd <- new_list
#         for (j in (1:nrow(line_keywords)))
#         {
#           if(line_keywords$kwd[j] == word)
#           {
#             line_keywords$kwd[j] == new_word
#             print('replaced')
#             print(word)
#             print('for')
#             print(new_word)
#           }
#         }
#         # line_keywords$kwd <- gsub(keyword, toupper(trim(new_word)), line_keywords$kwd, ignore.case = T)
      }

      new_kwd <- (paste(line_keywords$kwd, collapse = '; '))
      df$kwd[i] <- (new_kwd)
    }
  }
  print(nrow(df))
  return (df)
}

glossario_abstracts <- function(df)
{
  setwd( "C:/Users/bigda/Documents/Laraia_Wesley/Conectividade_Carros_Eletricos/Arquivos_Cos_Filtrados")
  glossary <- readRDS(file = "glossary.rds")
  colnames(glossary) <- c("component" ,   "word_aux"     ,    "action"    ,   "altered_word")
  glossary <- glossary %>% filter(component == 'resumo')
  words_to_delete <- glossary %>% select(word_aux) %>% filter(glossary$action == 'delete')
  words_to_delete <- as.character(words_to_delete$word_aux)
  words_to_change <- glossary %>% select(word_aux) %>% filter(glossary$action == 'replace')
  words_to_change <- as.character(words_to_change$word_aux)
  
  
  # words_to_delete <- toupper(words_to_delete)
  # words_to_change <- toupper(words_to_change)
  
  # df <- transform(df, kwd = as.character(kwd))
  #Rename the unique column to keywords to use it throughout the function
  for (i in (1:nrow(df)))
  {
      # words_to_delete <-  c(words_to_delete, 'CONCEPT')
      # i <- 1
      line_keywords <- df$AB[i]
  
      for (word in words_to_delete)
      {
        line_keywords <- gsub(word, '', line_keywords, ignore.case = T)
      }
                                 
      for (word in words_to_change)
      {
        keyword <- toupper(trim(word))
       
        replacement <- glossary[(glossary$action == 'replace'),] 
        replacement <- (replacement[(toupper(trim(replacement$word_aux)) == keyword),] )
        new_word <- as.character(replacement$altered_word)
        line_keywords <- gsub(keyword, toupper(trim(new_word)), line_keywords, ignore.case = T)
      }

      df$AB[i] <- line_keywords
    
  }
  return (df)
}

# Introduzindo as funcoes modificadas
# Conjunto de Funcoes adaptadas
# Incluidas: summary.bib, plot.bib, domin.bib, localcit.bib, histNet.bib, localcit.bib,
#            analise.bib, semati.bib, biblionet, identCor
#
# *************************************************************************************
#
# ++++++++++++++++++++++++++++++++++ SUMMARY ++++++++++++++++++++++++++++++++++
# Summary adaptada - adaptacao do metodo de sumarizar para objetos *.bib
summary.bib = function (object, ...) 
{
  if (class(object) != "bibliometrix") {
    cat("\n argument \"object\" tem que ser um objeto da classe \"bibliometrix\"\n")
    return(NA)
  }
  arguments <- list(...)
  if (sum(names(arguments) == "k") == 0) {
    k = 10 # default
  }
  else {
    k = arguments$k
  }
  if (sum(names(arguments) == "pause") == 0) {
    pause = FALSE
  }
  else {
    pause = arguments$pause
  }
  Co = NULL
  AC = NULL
  a<-rep("a", 12)
  MainInfo<-data.frame(cbind(a,a), stringsAsFactors=FALSE)
  colnames(MainInfo) <- c("Resumo Geral - Objeto", "Quantidade")
  MainInfo[1,1] = "Artigos"
  MainInfo[1,2] =  as.character(object$Articles)
  MainInfo[2,1] = "Fontes (Periodicos, Livros, etc.)"
  MainInfo[2,2] =  as.character(length(object$Sources))
  MainInfo[3,1] = "Termos_chave Adicionais (ID)" 
  MainInfo[3,2] =  as.character(length(object$ID))
  MainInfo[4,1] = "Termos_chave do Autor (DE)" 
  MainInfo[4,2] =  as.character(length(object$DE))
  MainInfo[5,1] = "Periodo" 
  MainInfo[5,2] = paste(min(object$Years, na.rm = T), "-", max(object$Years, 
                                                               na.rm = T))
  TCm = format(mean(as.numeric(object$TotalCitation), na.rm = TRUE), 
               digits = 4)
  MainInfo[6,1] = "Media de citacoes por artigo" 
  MainInfo[6,2] =  as.character(TCm)
  MainInfo[7,1] = "Autores" 
  MainInfo[7,2] =  as.character(object$nAuthors)
  MainInfo[8,1] = "Autores sem co-autores" 
  MainInfo[8,2] =  as.character(object$nAuthors - object$AuMultiAuthoredArt)
  MainInfo[9,1] = "Autores com co-autores" 
  MainInfo[9,2] =  as.character(object$AuMultiAuthoredArt)
  MainInfo[10,1] = "Media de artigos por autor" 
  MainInfo[10,2] = as.character(format(object$Articles/object$nAuthors, digits = 3))
  MainInfo[11,1] = "Media geral de autores por artigo (Naut/Nart)" 
  MainInfo[11,2] = as.character(format(object$nAuthors/object$Articles, digits = 3))
  MainInfo[12,1] = "Numero medio de autores em cada artigo" 
  MainInfo[12,2] = as.character(format(mean(object$nAUperPaper), digits = 3))
  #cat(MainInfo)
  if (pause == TRUE) {
    cat("Clique <Enter> para a nova tabela: ")
    line <- readline()
  }
  #cat("\nProducao Cientifica Anual\n\n")
  Y = data.frame(table(object$Years))
  names(Y) = c("Ano   ", "Artigos")
  #print(Y, row.names = FALSE)
  #cat("\n")
  ny = dim(Y)[1]
  GR = ((Y[ny, 2]/Y[1, 2])^(1/(ny - 1)) - 1) * 100
  #cat("Taxa Percetual de crescimento Anual", GR, "\n\n")
  if (pause == TRUE) {
    cat("Clique <Enter> para a nova tabela: ")
    line <- readline()
  }
  #cat("\nAutores Mais Produtivos\n\n")
  A = data.frame(cbind(object$Authors[1:k]))
  A$MPA = row.names(A)
  A = A[, c(2, 1)]
  A[, 3:4] = object$AuthorsFrac[1:k, ]
  names(A) = c("Autores       ", "Artigos", "Autores       ", 
               "Artigos Fracionados")
  A = format(A, justify = "left", digits = 3)
  row.names(A) = 1:k
  #print(A, row.names = TRUE)
  #cat("\n")
  if (pause == TRUE) {
    cat("Clique <Enter> para a nova tabela: ")
    line <- readline()
  }
  #cat("\nArtigos mais Citados\n\n")
  MostCitedPapers = object$MostCitedPapers[1:k, ]
  MostCitedPapers = format(MostCitedPapers, justify = "left", 
                           digits = 3)
  row.names(MostCitedPapers) = 1:k
  #print(MostCitedPapers, row.names = TRUE)
  #cat("\n")
  if (pause == TRUE) {
    cat("Clique <Enter> para a nova tabela: ")
    line <- readline()
  }
  kk = k
  if (!is.null(object$Countries)) {
    #cat("\nPaises mais Produtivos (do autor de correspondecia)\n\n")
    if (length(object$Countries) < k) {
      kk = length(object$Countries)
    }
    object$Countries = as.array(object$Countries)
    Co = data.frame(object$Countries[1:kk])
    Co$Country = row.names(Co)
    names(Co) = c("Pais  ", "Artigos", "Freq")
    Co$Freq = as.numeric(Co[, 2])/sum(object$Countries)
    colnames(object$CountryCollaboration) <- c("Pais", "AUP", "AVP")
    Co = cbind(Co, object$CountryCollaboration[1:kk, 2:3])
    Co = format(Co, justify = "left", digits = 3)
    row.names(Co) = 1:kk
    names(Co) = c("Pais  ", "Artigos", "Freq", "AUP", "AVP")
    #print(Co, row.names = TRUE)
    #cat("\n")
    #cat("\nAUP: Autores de um Unico Pais\n\nAVP: Autores de Varios Paises \n\n")
    if (pause == TRUE) {
      cat("Clique <Enter> para a nova tabela: ")
      line <- readline()
    }
    #cat("\nTotal de Citacoes por Pais\n\n")
    ind = which(!is.na(object$TotalCitation))
    AC = aggregate(object$TotalCitation[ind], list(object$CO[ind]), 
                   "sum")
    CC = object$Countries[sort(row.names(object$Countries))]
    CC2 = intersect(AC[, 1], rownames(CC))
    AC$Articles = object$Countries[CC2]
    AC = AC[order(-AC[, 2]), ]
    AC = AC[, c(1, 3, 2)]
    AC$TCperArticles = AC[, 3]/AC[, 2]
    AC = AC[, -2]
    names(AC) = c("Pais     ", "Citacoes Totais", "Media de Citacoes por Artigo")
    AC = format(AC, justify = "left", digits = 3)[1:kk, ]
    row.names(AC) = 1:kk
    #print(AC, row.names = TRUE)
    #cat("\n")
    if (pause == TRUE) {
      cat("Clique <Enter> para a nova tabela: ")
      line <- readline()
    }
  }
  if (!is.null(object$Sources)) {
    #cat("\nFontes Mais Relevantes\n\n")
    kk = k
    if (length(object$Sources) < k) {
      kk = length(object$Sources)
    }
    AA = data.frame(cbind(object$Sources[1:kk]))
    AA$MPA = row.names(AA)
    AA = AA[, c(2, 1)]
    names(AA) = c("Fontes       ", "Artigos")
    AA = format(AA, justify = "left", digits = 3)
    row.names(AA) = 1:kk
    #print(AA, row.names = TRUE)
    #cat("\n")
    if (pause == TRUE) {
      cat("Clique <Enter> para a nova tabela: ")
      line <- readline()
    }
  }
  if (!is.null(object$ID) & !is.null(object$DE)) {
    #cat("\nTermos-Chave Mais Relevantes\n\n")
    AAA = data.frame(cbind(object$DE[1:k]))
    AAA$MPA = row.names(AAA)
    AAA = AAA[, c(2, 1)]
    names(AAA) = c("DE Termos-chave", "Artigos")
    A2 = data.frame(cbind(object$ID[1:k]))
    A2$MPA = row.names(A2)
    A2 = A2[, c(2, 1)]
    AAA[, c(3, 4)] = A2
    names(AAA) = c("Termos-chave do Autor (DE)", "Artigos", 
                   "Termos-chave adics. (ID)", "Artigos")
    AAA = format(AAA, justify = "left", digits = 3)
    row.names(AAA) = 1:k
    #print(AAA, row.names = TRUE)
    #cat("\n")
  }
  summaryresults = list(MainInformation = MainInfo, AnnualProduction = Y, 
                        AnnualGrowthRate = GR, MostProdAuthors = A, MostCitedPapers = MostCitedPapers, 
                        MostProdCountries = Co, TCperCountries = AC, MostRelSources = AA, 
                        MostRelKeywords = AAA)
  invisible(summaryresults)
}

# ***********************************************************************************
#
# ++++++++++++++++++++++++++++++++++ PLOT ++++++++++++++++++++++++++++++++++
# Adaptacao da funcao plot (metodo plot pra objetos *.bib)
plot.bib = function (x, CoN, ...) 
{
  if (class(x) != "bibliometrix") {
    cat("\n argument \"x\" tem que ser um objeto da classe \"bibliometrix\"\n")
    return(NA)
  }
  arguments <- list(...)
  if (sum(names(arguments) == "k") == 0) {
    k = 10
  }
  else {
    k = arguments$k
  }
  if (sum(names(arguments) == "pause") == 0) {
    pause = FALSE
  }
  else {
    pause = arguments$pause
  }
  if (pause == TRUE) {
    cat("Clique <Enter> para o novo grafico: ")
    line <- readline()
  }
  
  #	    xx = as.data.frame(x$Authors[1:k])
  #       g1 <<- ggplot(data = xx, aes(x = xx$AU, y = xx$Freq)) + geom_bar(stat = "identity", 
  #        fill = "steelblue") + labs(title = "Most productive Authors", 
  #        x = "Authors") + labs(y = "N. of Documents") + theme_minimal() + 
  #       coord_flip()
  
  
  xx = as.data.frame(x$Authors[1:k])
  #print(dim(xx))
  #print(c(length(xx$AU), length(xx$Freq)))
  
  g1 <<- ggplot(data = xx, aes(x = xx$AU, y = xx$Freq)) + geom_bar(stat = "identity", 
                                                                   fill = "steelblue") + labs(title = "Autores Mais Produtivos", 
                                                                                              x = "Autores", y = "N. de Documentos") + theme_minimal() + 
    coord_flip()
  plot(g1)
  if (pause == TRUE) {
    cat("Clique <Enter> para o novo grafico: ")
    line <- readline()
  }
  
  AU_CO <- CoN$AU_CO
  AU_CO_df <- as.data.frame(AU_CO)
  join_all <- paste(AU_CO_df$AU_CO , collapse =";")
  split_all <- str_split(join_all,pattern = ';')[[1]]
  unique_countries <- split_all[!duplicated(split_all)]
  unique_countries <- sort(unique_countries)
  
  
  zeros <- c(rep(c(0), times = c(length(unique_countries))*2))
  avp <- rep("AVP", length(unique_countries))
  aup <- rep("AUP", length(unique_countries))
  unique_countries <- c(unique_countries,unique_countries)
  colab <- c(avp,aup)
  xx <- data.frame(unique_countries, zeros, colab) 
  names(xx) = c("Country", "Freq", "Collaboration")
  
  
  for (i in (1:length(AU_CO_df$AU_CO)))
  {
    country <- AU_CO_df$AU_CO[i]
    # print(country)
    split_list <- str_split(country,pattern = ';')[[1]]
    unique_countries2 <- split_list[!duplicated(split_list)]
    # print(length(unique_countries2))
    
    if(length(unique_countries2) == 1)
    {
      # print(unique_countries2)
      rowNumber <- as.numeric(which(xx$Country == unique_countries2 & xx$Collaboration == 'AUP'))
      # print(rowNumber)
      oldValue <- as.numeric(xx$Freq[rowNumber])
      xx$Freq[rowNumber] <- oldValue + 1
      # print("ROW NUMBER ##########################################")
    }else
    {
      for (country in unique_countries2)
      {
        # print(country)
        rowNumber <- as.numeric(which(xx$Country == country & xx$Collaboration == 'AVP'))
        # print(rowNumber)
        oldValue <- as.numeric(xx$Freq[rowNumber])
        xx$Freq[rowNumber] <- oldValue + 1
        # print("ROW NUMBER ##########################################")
      }
      
    }
  }
  
  xx %>%
    group_by(Country) %>%
    summarize(sum(Freq)) -> highest_numbers
  
  highest_numbers %>% arrange(desc(`sum(Freq)`)) -> highest_numbers
  
  countries <- highest_numbers$Country[1:15]
  
  xx_filtered <- xx %>%
    filter(Country %in% countries)
  
  xx <- xx_filtered
  
  xx %>% arrange(desc(Freq)) -> xx
  xx$Collaboration <- relevel(xx$Collaboration, 'AVP')
  
  
  g2 <<- suppressWarnings(ggplot(data = xx , aes(x = reorder(xx$Country,xx$Freq),
                                                 y = xx$Freq, fill = xx$Collaboration)) + geom_bar(stat = "identity") +
                            scale_fill_discrete(name = "Colaboracao", breaks = c("AUP",
                                                                                 "AVP")) + labs(title = "Paises Mais Produtivos   ",
                                                                                                x = "Paises  ", y = "N. de Documentos", caption = "AUP: Autores de um Unico Pais, AVP: Autores de Varios Paises") +
                            theme_minimal() + theme(plot.caption = element_text(size = 9,
                                                                                hjust = 0.5, color = "blue", face = "italic")) + coord_flip())
  #(g2)
  
  if (pause == TRUE) {
    cat("Clique <Enter> para o novo grafico: ")
    line <- readline()
  }
  Tab = table(x$Years)
  Y = data.frame(Year = as.numeric(names(Tab)), Freq = as.numeric(Tab))
  names(Y) = c("Year", "Freq")
  g3 <<- ggplot(Y, aes(x = Y$Year, y = Y$Freq)) + geom_line() + 
    geom_area(fill = "darkgreen", alpha = 0.1) + labs(x = "Ano", 
                                                      y = "Artigos", title = "Producao Cientifica Anual") + 
    theme(text = element_text(color = "#444444"), panel.background = element_rect(fill = "gray92"), 
          panel.grid.minor = element_line(color = "#4d5566"), 
          panel.grid.major = element_line(color = "#586174"), 
          plot.title = element_text(size = 24), axis.title = element_text(size = 14, 
                                                                          color = "#555555"), axis.title.y = element_text(vjust = 1, 
                                                                                                                          angle = 0), axis.title.x = element_text(hjust = 0))
  #plot(g3)
  if (pause == TRUE) {
    cat("Clique <Enter> para o novo grafico: ")
    line <- readline()
  }
  Table2 = aggregate(x$TotalCitation, by = list(x$Years), length)
  Table2$xx = aggregate(x$TotalCitation, by = list(x$Years), mean)$x
  Table2$Annual = NA
  d = date()
  d = as.numeric(substring(d, nchar(d) - 3, nchar(d)))
  Table2$Years = d - Table2$Group.1
  Table2$Annual = Table2$xx/Table2$Years
  names(Table2) = c("Year", "N", "MeanTCperArt", "MeanTCperYear", 
                    "CitableYears")
  g4 <<- ggplot(Table2, aes(x = Table2$Year, y = Table2$MeanTCperYear)) + 
    geom_line() + geom_area(fill = "darkgreen", alpha = 0.1) + 
    labs(x = "Ano", y = "Citacoes", title = "Media Anual de Citacoes por Artigo ") + 
    theme(text = element_text(color = "#444444"), panel.background = element_rect(fill = "gray92"), 
          panel.grid.minor = element_line(color = "#4d5566"), 
          panel.grid.major = element_line(color = "#586174"), 
          plot.title = element_text(size = 24), axis.title = element_text(size = 14, 
                                                                          color = "#555555"), axis.title.y = element_text(vjust = 1, 
                                                                                                                          angle = 0), axis.title.x = element_text(hjust = 0))
  
  #plot(g4)
  if (pause == TRUE) {
    cat("Clique <Enter> para o novo grafico: ")
    line <- readline()
  }
  g5 <<- ggplot(Table2, aes(x = Table2$Year, y = Table2$MeanTCperArt)) + 
    geom_line() + geom_area(fill = "darkgreen", alpha = 0.1) + 
    labs(x = "Ano", y = "Citacoes", title = "Media de Citacoes Totais por Ano") + 
    theme(text = element_text(color = "#444444"), panel.background = element_rect(fill = "gray92"), 
          panel.grid.minor = element_line(color = "#4d5566"), 
          panel.grid.major = element_line(color = "#586174"), 
          plot.title = element_text(size = 24), axis.title = element_text(size = 14, 
                                                                          color = "#555555"), axis.title.y = element_text(vjust = 1, 
                                                                                                                          angle = 0), axis.title.x = element_text(hjust = 0))
  #plot(g5)
  
}

# ***********************************************************************************
#
# ++++++++++++++++++++++++++++++++++ DOMINANCE ++++++++++++++++++++++++++++++++++
# Funcao dominance adaptada que calcula a dominancia quanto a aparicao como primeiro autor
domin.bib = function (results, k = 10) 
{
  if (class(results) != "bibliometrix") {
    cat("\n argument \"results\" have to be an object of class \"bibliometrix\"\n")
    return(NA)
  }
  Nmf = table(results$FirstAuthors[results$nAUperPaper > 1])
  FA = names(Nmf)
  AU = names(results$Authors)
  Mnt = rep(NA, k)
  for (i in 1:length(FA)) {
    Mnt[i] = results$Authors[FA[i] == AU]
  }
  Dominance = Nmf/Mnt
  t = 0
  cont = 0
  D = data.frame(matrix(NA, k, 3))
  for (i in 1:length(FA)) {
    if (sum(AU[i] == FA) > 0) {
      cont = cont + 1
      D[cont, 1] = Dominance[AU[i] == FA]
      D[cont, 2] = results$Authors[i]
      D[cont, 3] = Nmf[AU[i] == FA]
      row.names(D)[cont] = AU[i]
    }
    if (cont == k) 
      break
  }
  D$RankbyArticles = 1:dim(D)[1]
  D = D[order(-D[, 1]), ]
  D$RankDF = 1:dim(D)[1]
  names(D) = c("Taxa Prim. Autor", "N. Total Artigos", "N. Prim. Autor",  
               "Ordem pelo Total", "Ordem pela Taxa")
  return(D)
}

#
# ***********************************************************************************
#
# localcitations adaptadas
localcit.bib = function (M, sep = ";") # localCitations
{
  H = histNet.bib(M, n = dim(M)[1], sep = sep)
  LCS = H$histData
  M = H$M
  rm(H)
  AU = strsplit(M$AU, split = ";")
  n = lengths(AU)
  df = data.frame(AU = unlist(AU), LCS = rep(LCS$LCS, n))
  AU = aggregate(df$LCS, by = list(df$AU), FUN = "sum")
  names(AU) = c("Autor", "CitacoesLocais")
  AU = AU[order(-AU$CitacoesLocais), ]
  CR = list(Authors = AU, Papers = LCS)
  return(CR)
}
#
# ***********************************************************************************
#
# histNetwork adaptada
# histNet.bib = function (M, n = 10, sep = ";") # histNetwork
# {
#   
#   if (M$DB[1] != "ISI") {
#     cat("\nInfelizmente essa funcao funciona apenas com arquivos da WoS\n\n")
#     return()
#   }
#   M = M[order(M$PY), ] # M$PY = Publication Year
#   N = dim(M)[1]
#   rows = c(1:N)
#   # SR ? uma variavel criada pela funcao readFiles (Bibliometrix) e inicialmente vazia
#   # portanto este campo o nao existira se o df M tiver vindo de outra fonte
#   
#   if (!("SR" %in% names(M))) {
#     M = metaTagExtraction(M, Field = "SR")
#   }
#   lCit = Matrix(0, N, N)
#   for (i in 1:N) {
#     if (i%%100 == 0 | i == N) 
#       #    cat("Artigos analisados ", i, "\n")
#       x = M$SR[i]
#     Year = M$PY[i]
#     pos = grep(x, M$CR[M$PY >= Year])
#     pos = rows[M$PY >= Year][pos]
#     # DI e onde fica o DOI quando disponivel
#     if ("DI" %in% names(M)) {
#       if (!is.na(M$DI[i])) {
#         pos2 = grep(M$DI[i], M$CR[M$PY >= Year], fixed = TRUE)
#         pos2 = rows[M$PY >= Year][pos2]
#         pos = unique(pos, pos2)
#       }
#     }
#     if (length(pos) > 0) {
#       lCit[i, pos] = 1
#     }
#   }
#   LCS = rowSums(lCit)
#   M$LCS = LCS
#   row.names(lCit) = colnames(lCit) = M$SR
#   s = sort(LCS, decreasing = TRUE)[n]
#   ind = which(LCS >= s)
#   lCit = lCit[ind, ind]
#   Y = M$PY[ind]
#   if (!("DI" %in% names(M))) {
#     M$DI = NA
#   }
#   df = data.frame(Paper = M$SR[ind], DOI = M$DI[ind], Year = Y, 
#                   LCS = LCS[ind], GCS = M$TC[ind], stringsAsFactors = F)
#   df = df[order(df$Year), ]
#   row.names(df) = paste(df$Year, rep("-", dim(df)[1]), 1:dim(df)[1])
#   results = list(NetMatrix = t(lCit), Degree = s, histData = df, 
#                  M = M, LCS = LCS[ind])
#   return(results)
# }
#
# ***********************************************************************************
#
# NAO ESTA MAIS SENDO USADA. A FUNCAO ORIGINAL VOLTOU A SER USADA
# biblioAnalysis adaptada
analise.bib = function (M, sep = ";") # biblioAnalysis
{
  Authors = NULL
  Authors_frac = NULL
  FirstAuthors = NULL
  PY = NULL
  FAffiliation = NULL
  Affiliation = NULL
  Affiliation_frac = NULL
  CO = rep(NA, dim(M)[1])
  TC = NULL
  TCperYear = NULL
  SO = NULL
  Country = NULL
  DE = NULL
  ID = NULL
  MostCitedPapers = NULL
  Tags <- names(M)
  if ("AU" %in% Tags) {
    listAU = strsplit(as.character(M$AU), sep)
    listAU = lapply(listAU, function(l) trim(l))
    nAU = unlist(lapply(listAU, length))
    fracAU = unlist(sapply(nAU, function(x) {
      rep(1/x, x)
    }))
    AU = unlist(listAU)
    Authors = sort(table(AU), decreasing = TRUE)
    Authors_frac = aggregate(fracAU, by = list(AU), "sum")
    names(Authors_frac) = c("Author", "Frequency")
    Authors_frac = Authors_frac[order(-Authors_frac$Frequency)
                                ]
    FirstAuthors = lapply(listAU, function(l) l[[1]])
    listAUU = strsplit(as.character(M$AU[nAU > 1]), sep)
    AuMultiAuthoredArt = length(unique(gsub(" ", "", unlist(listAUU), 
                                            fixed = TRUE)))
  }
  if ("TC" %in% Tags) {
    #TC = as.numeric(M$TC)
    CT = as.numeric(M$TC)
    PY = as.numeric(M$PY)
    CurrentYear = as.numeric(format(Sys.Date(), "%Y"))
    CTporAno = CT/(CurrentYear - PY)
    if (sum(names(M) %in% "JI") == 1) {
      MostCitedPapers = data.frame(paste(M$AU, paste("(", 
                                                     M$PY, ")", sep = ""), M$JI, sep = ","), CT, CTporAno)
    }
    else {
      MostCitedPapers = data.frame(paste(M$AU, paste("(", 
                                                     M$PY, ")", sep = ""), M$SO, sep = ","), CT, CTporAno)
    }
    MostCitedPapers = MostCitedPapers[order(CT, decreasing = TRUE), 
                                      ]
    names(MostCitedPapers) = c("Paper         ", "CT", "CTporAno")
  }
  if ("ID" %in% Tags) {
    ID = tableTag(M, "ID", sep)
  }
  if ("DE" %in% Tags) {
    DE = tableTag(M, "DE", sep)
  }
  if ("SO" %in% Tags) {
    SO = gsub(",", "", M$SO, fixed = TRUE)
    SO = sort(table(SO), decreasing = TRUE)
  }
  if (("C1" %in% Tags) & (sum(!is.na(M$C1)) > 0)) {
    if (!("AU_UN" %in% Tags)) {
      M = metaTagExtraction(M, Field = "AU_UN")
    }
    AFF = M$AU_UN
    listAFF = strsplit(AFF, sep, fixed = TRUE)
    nAFF = unlist(lapply(listAFF, length))
    listAFF[nAFF == 0] = "NA"
    fracAFF = unlist(sapply(nAFF, function(x) {
      rep(1/x, x)
    }))
    AFF = trim.leading(unlist(listAFF))
    Affiliation = sort(table(AFF), decreasing = TRUE)
    Affiliation_frac = aggregate(fracAFF, by = list(AFF), 
                                 "sum")
    names(Affiliation_frac) = c("Affiliation", "Frequency")
    Affiliation_frac = Affiliation_frac[order(-Affiliation_frac$Frequency), 
                                        ]
    FAffiliation = lapply(listAFF, function(l) l[1])
    data("countries", envir = environment())
    countries = as.character(countries[[1]])
    M = metaTagExtraction(M, Field = "AU1_CO", sep)
    CO = M$AU1_CO
    Country = tableTag(M, "AU1_CO")
    SCP_MCP = countryCollaboration(M, Country, k = dim(Country), 
                                   sep)
  }
  results = list(Articles = dim(M)[1], Authors = Authors, AuthorsFrac = Authors_frac, 
                 FirstAuthors = unlist(FirstAuthors), nAUperPaper = nAU, 
                 Appearances = sum(nAU), nAuthors = dim(Authors), AuMultiAuthoredArt = AuMultiAuthoredArt, 
                 MostCitedPapers = MostCitedPapers, Years = PY, FirstAffiliation = unlist(FAffiliation), 
                 Affiliations = Affiliation, Aff_frac = Affiliation_frac, 
                 CO = CO, Countries = Country, CountryCollaboration = SCP_MCP, 
                 TotalCitation = CT, CTporAno = CTporAno, Sources = SO, 
                 DE = DE, ID = ID)
  class(results) <- "bibliometrix"
  return(results)
}
# NAO ESTA MAIS SENDO USADA. A FUNCAO ORIGINAL ESTA SENDO USADA
#
# ***********************************************************************************
#
# conceptualStructure adaptada
semant.bib = function (M, field = "ID", quali.supp = NULL, quanti.supp = NULL, 
                       minDegree = 2, k.max = 5, stemming = FALSE, labelsize = 3) 
{
  labelaxis <- labelsize + 1
  if (!is.null(quali.supp)) {
    QSUPP = data.frame(M[, quali.supp])
    names(QSUPP) = names(M)[quali.supp]
    row.names(QSUPP) = row.names(M)
  }
  if (!is.null(quanti.supp)) {
    SUPP = data.frame(M[, quanti.supp])
    names(SUPP) = names(M)[quanti.supp]
    row.names(SUPP) = row.names(M)
  }
  switch(field, ID = {
    CW <- cocMatrix(M, Field = "ID", type = "matrix", sep = ";")
    CW = CW[, colSums(CW) >= minDegree]
    CW = CW[, !(colnames(CW) %in% "NA")]
    CW = CW[rowSums(CW) > 0, ]
    CW = data.frame(apply(CW, 2, factor))
  }, DE = {
    CW <- cocMatrix(M, Field = "DE", type = "matrix", sep = ";")
    CW = CW[, colSums(CW) >= minDegree]
    CW = CW[rowSums(CW) > 0, ]
    CW = CW[, !(colnames(CW) %in% "NA")]
    CW = data.frame(apply(CW, 2, factor))
  }, ID_TM = {
    M = termExtraction(M, Field = "ID", remove.numbers = TRUE, 
                       stemming = stemming, language = "english", remove.terms = NULL, 
                       keep.terms = NULL, verbose = FALSE)
    CW <- cocMatrix(M, Field = "ID_TM", type = "matrix", 
                    sep = ";")
    CW = CW[, colSums(CW) >= minDegree]
    CW = CW[, !(colnames(CW) %in% "NA")]
    CW = CW[rowSums(CW) > 0, ]
    CW = data.frame(apply(CW, 2, factor))
  }, DE_TM = {
    M = termExtraction(M, Field = "DE", remove.numbers = TRUE, 
                       stemming = stemming, language = "english", remove.terms = NULL, 
                       keep.terms = NULL, verbose = FALSE)
    CW <- cocMatrix(M, Field = "DE_TM", type = "matrix", 
                    sep = ";")
    CW = CW[, colSums(CW) >= minDegree]
    CW = CW[, !(colnames(CW) %in% "NA")]
    CW = CW[rowSums(CW) > 0, ]
    CW = data.frame(apply(CW, 2, factor))
  }, TI = {
    M = termExtraction(M, Field = "TI", remove.numbers = TRUE, 
                       stemming = stemming, language = "english", remove.terms = NULL, 
                       keep.terms = NULL, verbose = FALSE)
    CW <- cocMatrix(M, Field = "TI_TM", type = "matrix", 
                    sep = ";")
    CW = CW[, colSums(CW) >= minDegree]
    CW = CW[, !(colnames(CW) %in% "NA")]
    CW = CW[rowSums(CW) > 0, ]
    CW = data.frame(apply(CW, 2, factor))
  }, AB = {
    M = termExtraction(M, Field = "AB", remove.numbers = TRUE, 
                       stemming = stemming, language = "english", remove.terms = NULL, 
                       keep.terms = NULL, verbose = FALSE)
    CW <- cocMatrix(M, Field = "AB_TM", type = "matrix", 
                    sep = ";")
    CW = CW[, colSums(CW) >= minDegree]
    CW = CW[rowSums(CW) > 0, ]
    CW = CW[, !(colnames(CW) %in% "NA")]
    CW = data.frame(apply(CW, 2, factor))
  })
  #
  if(any(nchar(colnames(CW))<=2)){
    CW <- CW[, -which(nchar(colnames(CW))<=2)] # inserida para consertar uma idiosincrasia "V8"
  }	
  #
  p = dim(CW)[2]
  quali = NULL
  quanti = NULL
  if (!is.null(quali.supp)) {
    ind = which(row.names(QSUPP) %in% row.names(CW))
    QSUPP = as.data.frame(QSUPP[ind, ])
    CW = cbind(CW, QSUPP)
    quali = (p + 1):dim(CW)[2]
    names(CW)[quali] = names(M)[quali.supp]
  }
  if (!is.null(quanti.supp)) {
    ind = which(row.names(SUPP) %in% row.names(CW))
    SUPP = as.data.frame(SUPP[ind, ])
    CW = cbind(CW, SUPP)
    quanti = (p + 1 + length(quali)):dim(CW)[2]
    names(CW)[quanti] = names(M)[quanti.supp]
  }
  res.mca <- MCA(CW, quanti.sup = quanti, quali.sup = quali, 
                 ncp = 2, graph = FALSE)
  coord = get_mca_var(res.mca)
  df = data.frame(coord$coord)[seq(2, dim(coord$coord)[1], 
                                   by = 2), ]
  row.names(df) = gsub("_1", "", row.names(df))
  if (!is.null(quali.supp)) {
    df_quali = data.frame(res.mca$quali.sup$coord)[seq(1, 
                                                       dim(res.mca$quali.sup$coord)[1], by = 2), ]
    row.names(df_quali) = gsub("_1", "", row.names(df_quali))
  }
  if (!is.null(quanti.supp)) {
    df_quanti = data.frame(res.mca$quanti.sup$coord)[seq(1, 
                                                         dim(res.mca$quanti.sup$coord)[1], by = 2), ]
    row.names(df_quanti) = gsub("_1", "", row.names(df_quanti))
  }
  a = fviz_nbclust(scale(df), kmeans, method = "silhouette", 
                   k.max = k.max)["data"]
  clust = as.numeric(a$data[order(-a$data$y), ][1, 1])
  km.res <- kmeans(scale(df), clust, nstart = 25)
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  b = fviz_cluster(km.res, data = df, labelsize = labelsize, 
                   repel = TRUE) + theme_minimal() + scale_color_manual(values = cbPalette[1:clust]) + 
    scale_fill_manual(values = cbPalette[1:clust]) + labs(title = "     ") + 
    geom_point() + theme(text = element_text(size = labelsize), 
                         axis.title = element_text(size = labelaxis, face = "bold"))
  if (!is.null(quali.supp)) {
    s_df_quali = df_quali[(abs(df_quali[, 1]) >= quantile(abs(df_quali[, 
                                                                       1]), 0.75) | abs(df_quali[, 2]) >= quantile(abs(df_quali[, 
                                                                                                                                2]), 0.75)), ]
    names(s_df_quali) = c("x", "y")
    s_df_quali$label = row.names(s_df_quali)
    x = s_df_quali$x
    y = s_df_quali$y
    label = s_df_quali$label
    b = b + geom_point(aes(x = x, y = y), data = s_df_quali, 
                       colour = "red", size = 1) + geom_text(aes(x = x, 
                                                                 y = y, label = label, size = labelsize/3), data = s_df_quali)
  }
  if (!is.null(quanti.supp)) {
    names(df_quanti) = c("x", "y")
    df_quanti$label = row.names(df_quanti)
    x = df_quanti$x
    y = df_quanti$y
    label = df_quanti$label
    b = b + geom_point(aes(x = x, y = y), data = df_quanti, 
                       colour = "blue", size = 1) + geom_text(aes(x = x, 
                                                                  y = y, label = label, size = labelsize/3), data = df_quanti)
  }
  #plot(b)
  semanticResults = list(net = CW, res.mca = res.mca, km.res = km.res, graf = b)
  return(semanticResults)
}
#
#
# Substituta para a function biblionetwork
biblionet = function (M, analysis = "coupling", network = "authors", sep = ";") 
{
  crossprod <- Matrix::crossprod
  NetMatrix = NA
  if (analysis == "coupling") {
    switch(network, authors = {
      WA = cocMatrix(M, Field = "AU", type = "sparse", 
                     sep)
      WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                      sep)
      CRA = crossprod(WCR, WA)
      NetMatrix = crossprod(CRA, CRA)
    }, references = {
      WCR = Matrix::t(cocMatrix(M, Field = "CR", type = "sparse", 
                                sep))
      NetMatrix = crossprod(WCR, WCR)
    }, sources = {
      WSO = cocMatrix(M, Field = "SO", type = "sparse", 
                      sep)
      WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                      sep)
      CRSO = crossprod(WCR, WSO)
      NetMatrix = crossprod(CRSO, CRSO)
    }, countries = {
      WCO = cocMatrix(M, Field = "AU_CO", type = "sparse", 
                      sep)
      WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                      sep)
      CRCO = crossprod(WCR, WCO)
      NetMatrix = crossprod(CRCO, CRCO)
    })
  }
  if (analysis == "co-occurrences") {
    switch(network, authors = {
      WA = cocMatrix(M, Field = "AU", type = "sparse", 
                     sep)
      #				
      if(any(nchar(colnames(WA))<=2)){
        WA <- WA[, -which(nchar(colnames(WK))<=2)] # inserida para consertar uma idiosincrasia "V8"
      }	
      #								
      NetMatrix = crossprod(WA, WA)
    }, keywords = {
      WK = cocMatrix(M, Field = "ID", type = "sparse", 
                     sep)
      #				
      if(any(nchar(colnames(WK))<=2)){
        WK <- WK[, -which(nchar(colnames(WK))<=2)] # inserida para consertar uma idiosincrasia "V8"
      }	
      #								
      NetMatrix = crossprod(WK, WK)
    }, author_keywords = {
      WK = cocMatrix(M, Field = "DE", type = "sparse", 
                     sep)
      #				
      if(any(nchar(colnames(WK))<=2)){
        WK <- WK[, -which(nchar(colnames(WK))<=2)] # inserida para consertar uma idiosincrasia "V8"
      }	
      #				 
      NetMatrix = crossprod(WK, WK)
    }, titles = {
      WK = cocMatrix(M, Field = "TI_TM", type = "sparse", 
                     sep)
      #				
      if(any(nchar(colnames(WK))<=2)){
        WK <- WK[, -which(nchar(colnames(WK))<=2)] # inserida para consertar uma idiosincrasia "V8"
      }	
      #								
      NetMatrix = crossprod(WK, WK)
    }, abstracts = {
      WK = cocMatrix(M, Field = "AB_TM", type = "sparse", 
                     sep)
      #				
      if(any(nchar(colnames(WK))<=2)){
        WK <- WK[, -which(nchar(colnames(WK))<=2)] # inserida para consertar uma idiosincrasia "V8"
      }	
      #								
      NetMatrix = crossprod(WK, WK)
    }, sources = {
      WSO = cocMatrix(M, Field = "SO", type = "sparse", 
                      sep)
      #				
      if(any(nchar(colnames(WK))<=2)){
        WSO <- WSO[, -which(nchar(colnames(WK))<=2)] # inserida para consertar uma idiosincrasia "V8"
      }	
      #								
      NetMatrix = crossprod(WSO, WSO)
    })
  }
  if (analysis == "co-citation") {
    switch(network, authors = {
      WA = cocMatrix(M, Field = "CR_AU", type = "sparse", 
                     sep)
      NetMatrix = crossprod(WA, WA)
    }, references = {
      WCR = cocMatrix(M, Field = "CR", type = "sparse", 
                      sep)
      NetMatrix = crossprod(WCR, WCR)
      A = row.names(NetMatrix)
      ind = unlist(regexec("*V[0-9]", A))
      A[ind > -1] = substr(A[ind > -1], 1, (ind[ind > -1] - 
                                              1))
      ind = unlist(regexec("*DOI ", A))
      A[ind > -1] = substr(A[ind > -1], 1, (ind[ind > -1] - 
                                              1))
      row.names(NetMatrix) = A
      colnames(NetMatrix) = A
    }, sources = {
      WSO = cocMatrix(M, Field = "CR_SO", type = "sparse", 
                      sep)
      NetMatrix = crossprod(WSO, WSO)
    })
  }
  if (analysis == "collaboration") {
    switch(network, authors = {
      WA = cocMatrix(M, Field = "AU", type = "sparse", 
                     sep)
      NetMatrix = crossprod(WA, WA)
    }, universities = {
      WUN = cocMatrix(M, Field = "AU_UN", type = "sparse", 
                      sep)
      NetMatrix = crossprod(WUN, WUN)
    }, countries = {
      WCO = cocMatrix(M, Field = "AU_CO", type = "sparse", 
                      sep)
      NetMatrix = crossprod(WCO, WCO)
    })
  }
  NetMatrix = NetMatrix[nchar(colnames(NetMatrix)) != 0, nchar(colnames(NetMatrix)) != 
                          0]
  return(NetMatrix)
}

