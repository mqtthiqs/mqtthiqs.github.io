---
layout: default
---

## Releases

{% assign sorted = site.releases | sort: 'date' | reverse %}
{% for m in sorted %}
<figure class="thumbnail">
  <a href="{{m.url}}"><img src="assets/img/{{m.cover}}" alt="{{m.title}}" /></a>
  <figcaption>
    <a href="{{m.url}}"><em>{{ m.title }}</em></a>
    <br />
    {{ m.label }}
    <br />
    ({{m.medium}}, {{m.date|date: "%B %Y"}})
  </figcaption>
</figure>
{% endfor %}

## Live performances

- l'An Vert (Liège, Belgium, 19 Mar 2022)
- Périscope (Lyon, 15 Dec 2021)
- Hands in the Dark Festival (Besançon, 2 Dec 2021)
- Festival Présences Électronique, Studio 104, Maison de la Radio (Paris, 3 Mar 2020) _[Cancelled]_
- DAda (Toulouse, 26 Oct 2019)
- Cafe OTO (London, 4 Oct 2019)
- Espace B (Paris, 3 Oct 2019)
- Festival We Hate Rock'n'Roll (Tincques, 24 Sept 2019)
- Cafe Collective (Aubervilliers, 7 Jun 2019)
- Station Station (Paris, 11 Jun 2019)
- Librairie Myriagone (Angers, 23 Feb 2019)
- Festival Présences, Maison de la Radio (Paris, 16 Feb 2019)
- Quasi-stellar Object #2, 100ECS (Paris, Oct 2018)
- Hylé night, 100ECS (Paris, Jul 2018)
- Marcel Festival, Péniche Adélaïde (Paris, Jun 2018)
- Café OTO (London, May 2018)
- Péripate (Génie d'Alex, Paris, Nov 2017)
- BASE/Modularsquare (Café de la Presse, Paris, Jul 2016)
- VENIN (l'Amour, Les Lilas, 2016)
- CKUT radio (Montréal, 2015)

## Miscellaneous

- Live modular synthesizer generative soundtrack composition and
  programming for [Sylvain Couzinet Jacques](http://www.couzinetjacques.com/) _"Sub Rosa"_ multimedia installation at
  Gallery [C/O Berlin](https://www.co-berlin.org/co-berlin-talent-award-0). Winner of the C/O Berlin Talent Award, supported by
  Institut Français and Ministère de la Culture et de la communication.
- Instrument design and programming for [_Textility of Making_](https://julienboudart.net/?page_id=586), premiered at [Festival Météo](https://ajc-jazz.eu/meteo-mulhouse-music-festival-2019/), Mulhouse, 31 Aug 2019
-  Live performance and interview on [Marie La Nuit](http://stationstation.fr/marie-la-nuit/)
- Mix for the [Riots in Brixton](https://www.mixcloud.com/RiotsInBrixton/riots-in-brixton-scene-35-matthias-puech-290118) podcast, commissioned by Vito Lucente (Jan. 18)    
- Mix for [Hands On Deck](https://www.mixcloud.com/lylradio/hands-on-deck-8/) on LYL radio, commissioned by Hands in the Dark
  Records (Oct 18)
