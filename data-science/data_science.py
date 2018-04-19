
# coding: utf-8

# In[1]:

import pandas as pd
import numpy as np

get_ipython().magic(u'matplotlib inline')
import matplotlib.pyplot as plt

#Figure Style
plt.style.use('ggplot')

#Import CSV
data = pd.read_csv("data/LAC-GSW 2015-11-04.csv")


# In[2]:

#Factoriser les champs texte
tmp,z = pd.factorize(data.player_name)
data.player_name = tmp
tmp,z = pd.factorize(data.team)
data.team = tmp

#Supprimer les colonnes inutiles 
data.drop('g_id', axis=1, inplace=True)


# In[3]:

#Calcule la distance cartésienne entre 2 points en feet (unité de référence originale de notre dataset)
#Convertit le résultat en mètre (1 foot = 0,3048m)

def dist(a,b):
    return np.sqrt((b[0]-a[0])**2 + (b[1]-a[1])**2) * 0.3048

#Calcule la distance en m entre les points d'une liste

def dist_list(X):
    ret = []
    for i in range(1,len(X)):
        ret.append(dist(X[i-1],X[i]))
    return ret

#Calcule la distance en m parcourue cumulée pour chaque joueur et la balle

def cum_dist_all():
    pd.options.mode.chained_assignment = None  # default='warn'
    T = []
    for i in data.player_name.unique():
        pi = data[data['player_name'] == i]
        Pi = list(zip(pi.x,pi.y))
        d = dist_list(Pi)
        d = np.cumsum(d)
        d = list(d)
        d = [0.0] + d
        pi['dist_cum'] = d
        T.append(pi)
    BT = pd.concat(T,axis=0)
    return BT

#Calcule la distance parcourue cumulée pour le joueur passé en paramètre (1 à 20)

def cum_dist_player(player):
    if player in range(1,21):
        pd.options.mode.chained_assignment = None  #prevent warning message
        T = []
        pi = data[data['player_name'] == player]
        Pi = list(zip(pi.x,pi.y))
        d = dist_list(Pi)
        d = np.cumsum(d)
        d = list(d)
        d = [0.0] + d
        pi['dist_cum'] = d
        T.append(pi)
        BT = pd.concat(T,axis=0)
        BT.drop('player_name', axis=1, inplace=True) 
        return BT
    else :
        return "Error player not defined"
    
#Retourne la somme totale parcourue pour un joueur passé en paramètre (1 à 20)

def sum_dist_player(player):
    if player in range(1,21):
        pi = data[data['player_name'] == player]
        Pi = list(zip(pi.x,pi.y))
        d = dist_list(Pi)
        return round(sum(d),2)
    return "Error player not defined"
    
#Retourne le temps de jeu d'un joueur passé en paramètre (1 à 20) arrondi au centième 

def time_played(player):
    if player in range(1,21):
        p = data[data['player_name'] == player]
        return round(list(p['time_remaining_total'])[0] - list(p['time_remaining_total'])[-1],2)
    return "Error player not defined"

#Calcul la vitesse moyenne d'un joueur passé en paramètre (1 à 20) 
#v = d/t -> vitesse en m/s 
#conversion en km/s -> v*3,6
#arrondi au centième 

def average_speed_player(player):
    return round(sum_dist_player(player)/time_played(player) * 3.6,2)

#Détermine le lieu sur le terrain (coordonnées dans le plan) où le joueur à le plus passé de temps en moyenne
def average_position_player(player):
    data[data['player_name'] == 1].mean()
    return (round(d['x'],1),round (d['y'],1))


# In[4]:

#Affiche un graphique qui compare les distances parcourues pour chaque joueur
def plot_dist_player():
    l = []
    for player in range(1,21):
        l.append((sum_dist_player(player)))
    df = pd.DataFrame(l,index=['Stephen Curry','Festus Ezeli','Klay Thompson','Draymond Green',
                               'Harrison Barnes','JJ Redick','Chris Paul','DeAndre Jordan',
                               'Blake Griffin','Lance Stephenson','Shaun Livingston','Andre Iguodala',
                               'Paul Pierce','Marreese Speights','Jamal Crawford','Josh Smith','Leandro Barbosa',
                               'Austin Rivers','Wesley Johnson','James Michael McAdoo'],
                      columns=['Distance parcourue'])
    df.plot(kind='bar',figsize=(15, 10))

#Affiche un graphique qui compare les vitesses moyennes pour chaque joueur
def plot_average_speed():
    l = []
    for player in range(1,21):
        l.append((average_speed_player(player)))
    df = pd.DataFrame(l,index=['Stephen Curry','Festus Ezeli','Klay Thompson','Draymond Green',
                               'Harrison Barnes','JJ Redick','Chris Paul','DeAndre Jordan',
                               'Blake Griffin','Lance Stephenson','Shaun Livingston','Andre Iguodala',
                               'Paul Pierce','Marreese Speights','Jamal Crawford','Josh Smith','Leandro Barbosa',
                               'Austin Rivers','Wesley Johnson','James Michael McAdoo'],
                      columns=['Vitesse moyenne'])
    df.plot(kind='bar',figsize=(15, 10))

#Affiche un graphique avec les zones où le joueur à passer le plus de temps sur le terrain
def plot_positions_player(player):
    p = data[data['player_name'] == player]
    l = [p['x'],p['y']]
    df = pd.concat(l,axis=1)
    df.plot.hexbin(x='x', y='y', gridsize=10,figsize=(15, 10))


# In[5]:

# -------------------------------------- MAIN ------------------------------------- #

# Joueur 0 = La Balle
# Joueur 1 = Stephen Curry
# Joueur 2 = Festus Ezeli
# Joueur 3 = Klay Thompson
# Joueur 4 = Draymond Green
# Joueur 5 = Harrison Barnes
# Joueur 6 = JJ Redick
# Joueur 7 = Chris Paul
# Joueur 8 = DeAndre Jordan
# Joueur 9 = Blake Griffin
# Joueur 10 = Lance Stephenson
# Joueur 11 = Shaun Livingston
# Joueur 12 = Andre Iguodala
# Joueur 13 = Paul Pierce
# Joueur 14 = Marreese Speights
# Joueur 15 = Jamal Crawford
# Joueur 16 = Josh Smith
# Joueur 17 = Leandro Barbosa
# Joueur 18 = Austin Rivers
# Joueur 19 = Wesley Johnson
# Joueur 20 = James Michael McAdoo

# Team 0 = La balle
# Team 1 = Home
# Team 2 = Away

#----------Décommentez les lignes pour tester -----------#

#Tableau de distance cumulée parcourue pour chaque joueur + la balle
#cum_dist_all()

#Tableau de distance cumulée parcourue pour Klay Thompson
#cum_dist_player(3)

#Distance Totale parcourue pour Stephen Curry
#sum_dist_player(1)

#Temps total de jeu pour Paul Pierce
#time_played(13)

#Vitesse moyenne de Lance Stephenson
#average_speed_player(10)

#Graphique de distance parcourue pour chaque joueur
#plot_dist_player()

#Graphique de vitesse moyenne parcourue pour chaque joueur
#plot_average_speed()

#James Michael McAdoo n'a joué que 9,58 secondes (surement en sprint) 
#et a parcourue 64,96m d'où la vitesse moyenne élevée non représentatif :
#sum_dist_player(20)
#time_played(20)

#Zones d'influence de Stephen Curry
#plot_positions_player(1)


# In[ ]:



