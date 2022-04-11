
module Ficha3 where

import Ficha1
-- Exercicio 3
type Etapa = (Horas,Horas)
type Viagem = [Etapa]

--a)
etapaValida :: Etapa -> Bool
etapaValida ((H hp mp) , (H hc mc)) = horaValida4 (H hp mp) && horaValida4 (H hc mc) && (hc>=hp) && (mc>mp)

-- b)
viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [x] = True 
viagemValida (h:e:t)= etapaValida h && etapasConsecutivas h e && viagemValida (e:t)

etapasConsecutivas :: Etapa -> Etapa -> Bool 
etapasConsecutivas (_,chegada) (partida,_) = etapaValida (chegada,partida)

-- c)
partidaChegada :: Viagem -> Etapa
partidaChegada 