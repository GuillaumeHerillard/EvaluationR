library(RMySQL)
library(tidyverse)
library(shiny)
con <- dbConnect(MySQL(), host="localhost", user = "root",password="root", dbname="evaluation")
dbGetQuery(con, "SET NAMES 'utf8'")
quest_rep <- tbl(con,sql("SELECT question.libelle, reponse.session_id,
CONCAT(prenom.texte, ' ', nom.texte) as nom_prenom,
formation.libelle as formation,
question.type,
session.date as session_date,
case 
when score is not null then score 
when reponse.texte is not null then reponse.texte
when reponse.date is not null then reponse.date
when choix_id is not null then choix.libelle
end as reponse
from question 
inner join reponse 
on question.id = question_id
left join choix
on choix_id = choix.id inner join(
select session_id,texte from reponse 
inner join question 
on question.id = question_id
where libelle = 'Prénom') as prenom on prenom.session_id = reponse.session_id
inner join(
select session_id,texte from reponse 
inner join question 
on question.id = question_id
where libelle = 'nom') as nom on nom.session_id = reponse.session_id
inner join(select session_id, choix.libelle from reponse 
inner join question on question.id = reponse.question_id
left join choix on choix.id = choix_id
where question.libelle = 'Vous avez terminé une formation de') as formation
on reponse.session_id = formation.session_id 
inner join session on session.id = reponse.session_id"))%>% 
  collect()


