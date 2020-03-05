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
