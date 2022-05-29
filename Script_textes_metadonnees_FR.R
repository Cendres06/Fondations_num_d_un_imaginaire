library(rvest)
library(stringr)
library(crul)
library(htmltidy)
library(lubridate)
library(ggplot2)

#récupérer les tags
recup_tag = function(page_lien){
  #Fonctionnelle. Peut être améliorée avec l'extraction par expressions régulières, les miennes sont brouillonnes.
  page_html = read_html(page_lien, encoding = "UTF-8")
  code_page_tags <- page_html %>% html_element(".page-tags")
  text_page_tags <- as.character(code_page_tags)
  text_moins_de_css <- str_extract(text_page_tags,"(<a href.*?</a>(?!<a href))")
  tags_division <- strsplit(text_moins_de_css,"</a>")
  tags_dones <- str_extract(tags_division[[1]],"(?<=>)(.*)")
  
  return(tags_dones)
}

#récupérer le contenu du texte
recup_body_fr = function(page_lien){
  t_depart = Sys.time()
  #Meilleure reconnaissance des dates selon le format DD/JJ/AA ou DD mois AA ?
  #Plutôt que de nettoyer grâce à des expressions régulières, utiliser html_text() ?
  
  page_html = read_html(page_lien,encoding = "UTF-8")
  
  code_page_corps <- page_html %>% html_element("#page-content")
  
  html_in_str <-as.character(code_page_corps)
  
  #on récupère les métadonnées du module de crédit
    #pour cela, on doit évacuer les \ qui posent problème à R
  html_recevable = gsub(pattern = "\"|\n|\t",replacement = "", x = html_in_str)
  metadonnees <- str_extract(html_recevable,"(?<=<h2><span>Crédits</span></h2>)(.*?)(?=credit-back)")
  
  #on crée la matrice nécessaire
  m_donnees = matrix(nrow=1,ncol=13)
  m_donnees[1,3]="Non pertinent"
  m_donnees[1,5]="Non pertinent"
  m_donnees[1,12]=page_lien
  colnames(m_donnees)<-c("Titre","Auteur·rice·(s)","Traducteur·rice·(s)","Date de création","Date de traduction","Informations relatives aux images","Remerciements","Commentaires","Inclassable","Etiquettes","Texte","Lien","Temps requête")
  
  #s'il n'y a pas de module de crédit
  if (is.na(metadonnees)){
    m_donnees[1,1]="Module de crédit absent"
    m_donnees[1,2]="Module de crédit absent"
    m_donnees[1,4]="Module de crédit absent"
    m_donnees[1,6]="Module de crédit absent"
    m_donnees[1,7]="Module de crédit absent"
    m_donnees[1,8]="Module de crédit absent"
    m_donnees[1,9]="Module de crédit absent"
  }else{
    metadonnees_division <- strsplit(metadonnees,"<br>")
    
    #On nettoie les métadonnées
    
    for (i in c(1:length(metadonnees_division))){
      metadonnees_division[[i]] <- gsub("<.*?>","",metadonnees_division[[i]])
    }
    
    champ_et_rep = c(gsub("<.*","",metadonnees_division[[1]]))
    champ_et_rep_separe = c(strsplit(champ_et_rep," : "))
    
    #on place les données dans la matrice
    
    for(i in 1:length(champ_et_rep_separe)){
      if(regexpr("[Tt]itre",champ_et_rep_separe[[i]][1])!=-1){
        m_donnees[,1] = champ_et_rep_separe[[i]][2]
      }else if(regexpr("[Aa]uteur|[Aa]utrice|[EÉ]crit par",champ_et_rep_separe[[i]][1])!=-1){
        m_donnees[,2] = champ_et_rep_separe[[i]][2]
      }else if(regexpr("[dD]ate|[EÉ]crit le|[Pp]ubli",champ_et_rep_separe[[i]][1])!=-1){
        m_donnees[,4] = champ_et_rep_separe[[i]][2]
      }else if(regexpr("[iI]mage",champ_et_rep_separe[[i]][1])!=-1){
        m_donnees[,6] = champ_et_rep_separe[[i]][2]
      }else if(regexpr("[Mm]erci",champ_et_rep_separe[[i]][1])!=-1){
        m_donnees[,7] = champ_et_rep_separe[[i]][2]
      }else if(regexpr("[Cc]ommentaire",champ_et_rep_separe[[i]][1])){
        m_donnees[,8] = champ_et_rep_separe[[i]][2]
      } else {
        m_donnees[,9] = champ_et_rep_separe[[i]][2]
      }
    }
  }
  
  #On récupère le texte de la page
  code_page_text <- code_page_corps %>% html_elements("p")
  page_texte <- html_text(code_page_text)
  texte <- page_texte[2]
  if(length(page_texte)>2){
    for(i in 3:length(page_texte)){
      texte <- paste(texte,page_texte[i],sep = "\n")
    }
  }
  
  #ajout à la matrice
  m_donnees[,11]=texte
  
  #étiquettes
  v_etiquettes <- recup_tag(page_lien)
  etiquettes <- v_etiquettes[1]
  if(length(v_etiquettes)>1){
    for(i in 2:length(v_etiquettes)){
      etiquettes <- paste(etiquettes,v_etiquettes[i],sep = " ; ")
    }  
  }
  #ajout des étiquettes
  m_donnees[10] <- etiquettes
  
  #on ne veut pas avoir de NA dans les cases de la matrice
  index_na = is.na(m_donnees)
  m_donnees[index_na]="Vide"
  
  #Obligé de ne pas prendre en compte ça dans le calcul du temps
  df <- data.frame(m_donnees)
  t_fin = Sys.time()
  df[13] <- t_fin - t_depart
  
  return(df)
}

#on identifie tous les écrits qui sont des contes, peu importe la branche d'origine
base_donnees_contes = function(){
  page_html_des_contes = read_html("http://fondationscp.wikidot.com/system:page-tags/tag/conte#pages")
  code_page_corps_contes <- page_html_des_contes %>% html_elements(".pages-list-item")
  html_page_contes_in_str <-as.character(code_page_corps_contes)
  html_page_contes_recevable = gsub(pattern = "\"|\n|\t",replacement = "", x = html_page_contes_in_str)
  liste_liens_contes = str_extract_all(html_page_contes_recevable ,'(?<=a href=)(.*?)(?=>)')
  return(liste_liens_contes)
}

#on identifie tous les écrits qui sont des rapports, peu importe la branche d'origine
base_donnees_rapports = function(){
  page_html_des_rapports = read_html("http://fondationscp.wikidot.com/system:page-tags/tag/scp#pages")
  code_page_corps_rapports <- page_html_des_rapports %>% html_elements(".pages-list-item")
  html_page_rapports_in_str <-as.character(code_page_corps_rapports)
  html_page_rapports_recevable = gsub(pattern = "\"|\n|\t",replacement = "", x = html_page_rapports_in_str)
  liste_liens_rapports = str_extract_all(html_page_rapports_recevable ,'(?<=a href=)(.*?)(?=>)')
  return(liste_liens_rapports)
}

#on recherche toutes les pages étiquettées avec un tag particulier, en récupérant leurs métadonnées telles que consignées par les utilisateurices
recherche_liste_tag_fr = function(lien_liste_page, bd_contes = base_donnees_contes(), bd_rapports= base_donnees_rapports(), affiche_temps=FALSE){
  t_depart = Sys.time()
  #ne sélectionner que les pages contes et rapport
  
  #Liste du tag recherché
  page_html_de_tags = read_html(lien_liste_page, encoding = "UTF-8")
  
  code_page_corps <- page_html_de_tags %>% html_elements(".pages-list-item")
  
  html_in_str <-as.character(code_page_corps)
  
  html_recevable = gsub(pattern = "\"|\n|\t",replacement = "", x = html_in_str)
  liste_liens = str_extract_all(html_recevable ,'(?<=a href=)(.*?)(?=>)')
  
  index = (liste_liens %in% bd_contes | liste_liens %in% bd_rapports)
  
  liste_liens = liste_liens[index]
  
  lien = paste("http://fondationscp.wikidot.com",liste_liens[[1]],sep = "")
  donnees = recup_body_fr(lien)
  
  if(length(liste_liens)>1){
    for(i in 2:length(liste_liens)){
      lien = paste("http://fondationscp.wikidot.com",liste_liens[[i]],sep = "")
      donnees = rbind(donnees, recup_body_fr(lien))
    }
  }
  
  t_fin = Sys.time()
  if(affiche_temps){
    print(t_fin - t_depart)
  }
  return(donnees)
}

#On choisit le tag qui indique les textes francophones
html_liste_tag_fr = "http://fondationscp.wikidot.com/system:page-tags/tag/fr#pages"

#On crée la bdd des contes et celle des rapports
bdc = base_donnees_contes()
bdr = base_donnees_rapports()

#On crée notre base de données de textes et métadonnées sur lesquelles on va travailler
doudou = recherche_liste_tag_fr(html_liste_tag_fr, bd_contes = bdc, bd_rapports = bdr)

length(doudou)

#On fait la même opération, cette fois-ci en récupérant les métadonnées intrinsèques au site plutôt que de choisir les métadonnées ajoutées par les utilisateurices
recup_metadonnees = function(page_lien){
  #action-area est vide quand je récupère le html entier de la page CAR c'est une requête ajax qui permet à la page de générer les données à l'aide du javascript, or c'est cet élément qui contient les révisions pour les métadonnées.
  #On a donc regardé du côté des packages R permettant une interaction web.
  #Merci à Corentin POUPRY pour son aide précieuse dans la compréhension du principe et l'écriture du code
  #Lien GitHUB menant à son travail, adapté et repris ici : https://github.com/foundation-int-tech-team/sherlock/blob/e1d44d115cb6263c229b16ccfc66ebe846563e51/sherlock/utils/wikidot.py#L24
  t_depart = Sys.time()
  
  page_html = read_html(page_lien,encoding = "UTF-8")

  html_in_str <-as.character(page_html)
  
  #Pour créer un appel vers le serveur, il faut trouver l'id de la page concernée dans l'HTML
  page_id = str_extract(html_in_str,"WIKIREQUEST.info.pageId = [0-9]*;")
  page_id = str_extract(page_id,"[0-9]+")
  
  #On crée ensuite un client HTTP afin de gérer la requête
  client <- HttpClient$new(
    url = "http://fondationscp.wikidot.com/ajax-module-connector.php",
    opts = list(
      timeout = 2,#à augmenter si problème de timeout reached
      cookie = "wikidot_token7=c897dbbefa41b7e2e8df4b4cb96dfd2e"
    ),
    headers = list(
      "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8"
    )
  )

  #On effectue la requête POST en utilisant un token respectant le format nécessaire. Wikidot étant une passoire, le jeton généré n'a pas d'importance, seul le respect du format compte.
  res <- client$post(
    encode = "form",
    body = list(
      "perpage" = "99999",
      "page_id" = page_id,
      "options" = "{\"all\":true}",
      "moduleName" = "history/PageRevisionListModule",
      "callbackIndex" = "1",
      "wikidot_token7" = "c897dbbefa41b7e2e8df4b4cb96dfd2e"
    )
  )
  
  #On transforme le résultat (json) de manière à pouvoir accéder à l'élément body en R
  res <- jsonlite::fromJSON(res$parse(encoding = "UTF-8"))
  
  #On recherche grâce au xpath l'auteur
  #Si l'auteur est supprimé, un élément de class "printuser deleted" et de data-id "1944760" est présent dans le texte.
    
  auteur <- tidy_html(res["body"]) %>%
    read_html() %>% 
    html_nodes(xpath = "/html/body/table/tr[last()]/td/span/a[1]/img/@alt") %>%
    html_text()
  
  if(identical(auteur,character(0))){
    auteur <- "Auteurice supprimé·e"
  }
  
  #On recherche grâce au xpath le timestamp à la norme POSIX
  date_crea <- tidy_html(res["body"]) %>%
    read_html() %>% 
    html_nodes(xpath = "/html/body/table/tr[last()]/td[6]/span/@class") %>%
    html_text()
  
  date_crea <- str_extract(date_crea,"time_[0-9]+")
  date_crea <- str_extract(date_crea,"[0-9]+")
  
  #On crée le data.frame qui va accueillir ces données
  m_donnees = matrix(nrow=1,ncol=4)
  colnames(m_donnees)<-c("Lien URL","Auteur·ice","Date de création","Temps requête")
  m_donnees[,1] <- page_lien
  m_donnees[,2] <- auteur
  m_donnees[,3] <- date_crea
  
  df = data.frame(m_donnees)
  t_fin = Sys.time()
  df[4] <-t_fin - t_depart
  
  return(df)
}

#On récupère les métadonnées automatiques sur tous les liens du site
metadonnees_liste_tag = function(lien_liste_page, fr = TRUE, bd_contes = base_donnees_contes(), bd_rapports= base_donnees_rapports(),affiche_temps=FALSE){
  t_depart=Sys.time()
  #Liste du tag recherché
  page_html_de_tags = read_html(lien_liste_page, encoding = "UTF-8")
  
  code_page_corps <- page_html_de_tags %>% html_elements(".pages-list-item")
  
  html_in_str <-as.character(code_page_corps)
  
  html_recevable = gsub(pattern = "\"|\n|\t",replacement = "", x = html_in_str)
  liste_liens = str_extract_all(html_recevable ,'(?<=a href=)(.*?)(?=>)')
  
  index = (liste_liens %in% bd_contes | liste_liens %in% bd_rapports)
  
  liste_liens = liste_liens[index]
  
  lien = paste("http://fondationscp.wikidot.com",liste_liens[[1]],sep = "")
  donnees = recup_metadonnees(lien)
  
  if(length(liste_liens)>1){
    for(i in 2:length(liste_liens)){
      #print(i) #option qui permet de détecter les textes qui posent problème
      lien = paste("http://fondationscp.wikidot.com",liste_liens[[i]],sep = "")
      donnees = rbind(donnees, recup_metadonnees(lien) )
    }
  }
  
  t_fin = Sys.time()
  if(affiche_temps){
    print(t_fin - t_depart)  
  }
  return(donnees)
}

#BDD de textes avec métadonnées automatiques
meta_doudou = metadonnees_liste_tag(html_liste_tag_fr, bd_contes = bdc, bd_rapports = bdr)

#Calcul du temps moyen de création d'une requête
moyenne_requete_pour_variable=function(df){
  som = 0
  for(i in 1:nrow(df)){
    temps <- df[i,ncol(df)]
    som <- som + temps
  }
  return(som/nrow(df))
}

#Examen des performances des algorithmes
moyenne_requete_pour_variable(doudou)
moyenne_requete_pour_variable(meta_doudou)

#Voici une liste de néologismes propres à l'univers qu'on va rechercher dans les textes
liste_neologisme = c("amnésique","amnésiant","télékill","diacrinochrom","daevite","anart","SCP","humes","sarkiste","Mekhanite","cinquis","gendastre","singularme","orthothan","mémétique")
#On questionne la pertinence d'ajouter "hémovore" et "hémophage" à cette liste
#gendastre trouvera aussi les allusions à gendastrerie
#mémétique trouvera aussi les allusions à antimémétique
#cinquis trouvera les allusions cinquistes et cinquisme
#anart trouvera aussi les allusions à anartiste
#diacrinochrom trouvera les allusions à diacrinochrome et diacrinochromie

#Cherche le nombre d'occurrence d'un néologisme donné dans un texte donné
recherche_neologisme = function(neologisme,texte){
  ma_liste_de_mots <- unlist(strsplit(texte," "))
  expr_reg <- ""
  for (i in 1:nchar(neologisme)){
    char <- substring(neologisme,i,i)
    if (regexpr("[A-Z]",char)!=-1){
      char_reg <- paste("[",char,tolower(char),"]",sep="")
    } else if(regexpr("[a-z]",char)!=-1){
      char_reg <- paste("[",toupper(char),char,"]",sep="")
    } else if (regexpr("é",char)!=-1){
      char_reg <- paste("[eéEÉ]",sep="")
    } else if (regexpr("è",char)!=-1){
      char_reg <- paste("[eèEÈ]",sep="")
    } else {
      char_reg <- char
    }
    expr_reg = paste(expr_reg,char_reg,sep="")
  }
  ind <- (regexpr(expr_reg,ma_liste_de_mots)!=-1)
  return(sum(ind))
}


#Cherche le nombre d'occurrence d'un néologisme donné dans tous les textes de la BDD
all_recherche_neologisme=function(neologisme,textes=doudou[,11]){
  somme = 0
  for (i in 1:length(textes)){
    ajout = recherche_neologisme(neologisme,textes[i])
    somme <- somme + ajout
  }
  return(somme/length(textes))
}

#Compile pour tous les néologismes dans tous les textes
construction_donnees_presence = function(neologismes = liste_neologisme, textes=doudou[,11]){
  res = NULL
  for(i in 1:length(neologismes)){
    ajout = all_recherche_neologisme(neologismes[i],textes)
    res = c(res,ajout)
  }
  return(res)
}

donnees_presence= construction_donnees_presence()

#frequence d'un néologisme donné dans un texte donné
freq_neologisme= function(neologisme,texte){
  nb_total_mot = length(unlist(strsplit(texte," ")))
  nb_neo = recherche_neologisme(neologisme,texte)
  freq = nb_neo/nb_total_mot
  return(freq)
}

#frequence d'un néologisme donné dans tous les textes
all_freq_neologisme=function(neologisme,textes=doudou[,11]){
  #le calcul de la fréquence pour "amnésique" donne NaN
  # peut-être à cause du caractère spécial "վ" présent dans le texte ?
  #Problème "résolu" en transformant les variables NaN en 0, mais il faudrait creuser un peu plus
  somme = 0
  for (i in 1:length(textes)){
    ajout = freq_neologisme(neologisme,textes[i])
    if (is.nan(ajout)){
      ajout <- 0
    }
    somme <- somme + ajout
  }
  return(somme/length(textes))
}

#Compile pour tous les néologismes dans tous les textes
construction_donnees_frequence=function(neologismes = liste_neologisme, textes=doudou[,11]){
  res = NULL
  for(i in 1:length(neologismes)){
    res = c(res,all_freq_neologisme(neologismes[i],textes))
  }
  return(res)
}

donnees_freq = construction_donnees_frequence()

#On veut regarder la variance de nos moyennes, donc on construit une fonction intermédiaire
variance_recherche=function(neologisme,textes=doudou[,11], moyenne,fonction="null"){
  #print(neologisme)
  if(fonction=="freq"){
    donnee = all_freq_neologisme(neologisme,textes)
    return(donnee-moyenne)
  }else if(fonction=="presence"){
    donnee = all_recherche_neologisme(neologisme,textes)
    return(donnee-moyenne)
  }else{
    print("Le paramètre 'fonction' est erroné")
    return(0)
  }
}

#Puis on calcule la variance
construction_donnees_variance=function(neologismes = liste_neologisme, textes=doudou[,11], moyenne, fonction){
  res = 0
  for(i in 1:length(neologismes)){
    ajout = variance_recherche(neologismes[i],textes, moyenne,fonction)^2
    res = res + ajout
  }
  return(res/length(neologismes))
}

donnees_var_presence = construction_donnees_variance(moyenne = sum(donnees_presence)/length(donnees_presence),fonction = "presence")
donnees_var_presence
donnees_var_frequence = construction_donnees_variance(moyenne = sum(donnees_freq)/length(donnees_freq),fonction = "freq")
donnees_var_frequence

#Puis l'écart-type
ecart_type=function(neologismes = liste_neologisme, textes=doudou[,11],moyenne, fonction){
  return(sqrt(construction_donnees_variance(neologismes,textes,moyenne,fonction)))
}

et_freq = ecart_type(moyenne = sum(donnees_freq)/length(donnees_freq),fonction = "freq")
et_freq
et_presence = ecart_type(moyenne = sum(donnees_presence)/length(donnees_presence),fonction = "presence")
et_presence

#Un premier graphique pour observer la fréquence des néologismes dans les textes
#On multiplie les fréquences par 10 pour le confort visuel
df <- data.frame(x=liste_neologisme, y=donnees_freq*10)
ggplot(data = df, mapping = aes(x=x,y=y)) +
  geom_bar(stat="identity")

#Le pic de SCP et la grande variance est motif de questionnement.
#Hypothèse : "SCP" est extrêmement présent dans les rapports.
#Pour la tester, on va faire un graphique qui confronte cette idée en observant la fréquence
  #dans les contes et dans les rapports

index_conte = (regexpr("conte",doudou[,10])!=-1)

textes_contes = doudou[index_conte,11]
textes_contes[1]
textes_rapports = doudou[!index_conte,11]
textes_rapports[1]

donnees_frequence_conte = construction_donnees_frequence(textes = textes_contes)
donnees_frequence_conte
donnees_frequence_rapport = construction_donnees_frequence(textes = textes_rapports)
donnees_frequence_rapport

#Un deuxième graphique avec réduction dimensionnelle 3 vers 2
df_stats <- data.frame(x=donnees_freq,y=donnees_frequence_conte, z=donnees_frequence_rapport)
freq_mat_pca <- prcomp(df_stats[,1:3], scale = TRUE)
biplot(freq_mat_pca)
gdf=data.frame(noms=liste_neologisme, pc1=freq_mat_pca$x[,1], pc2=freq_mat_pca$x[,2])
ggplot(data = gdf, mapping = aes(x = pc1, y = pc2)) +
  geom_point(aes(color=noms), size=5) +
  scale_colour_hue(h=c(0, 300))
