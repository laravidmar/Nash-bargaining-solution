# Nash-bargaining-solution / Kooperativne igre 

## Opis kooperativnih iger 

V teoriji iger je kooperativna igra, igra pri kateri skupine igralcev med seboj tekmuje. Pri tem si vsak igralec želi maksimizirati svojo koristnost U. Igralci med seboj lahko delujejo usklajeno in morda izmenjajo koristnosti. To je del pogodbe, ki določi stranska plačila. Če se sporazumeta, dobi prvi igralec x, drugi pa y, kjer je (x, y) ∈ S, $S \subseteq \mathbf{R}^2$ pa je množica dopustnih sporazumov. Če
se ne sporazumeta, prvi dobi u, drugi pa v. Točka (u, v) je torej pogajalsko
izhodišče. Množica $S_{u,v}$ je konveksna in kompaktna. Nasheva rešitev tega modela pogajanja je, da v primeru, ko je množica $S_{u, v}$ prazna, igralca ostaneta v pogajalskem izhodišču (u, v) (status quo), sicer pa se sporazumeta za točko $(x, y) ∈ S_{u,v}$, kjer je produkt (x − u)(y − v) maksimalen. Obstaja natanko ena taka
točka $(x^{\star}, y^{\star})$ in ta se vedno nahaja na robu množice S.

Imamo lahko prenosljive dobrine in neprenosljive dobrine. Pri prenosljivih dobrinah so koristi igralcev med seboj neposredno primerljive in posledično lahko to koristnost igralci med seboj smiselno prenašajo. Dopustna množica je konveksna lupina ${(x - s,  y + s) \in S' : s \in \mathbf{R}},$ pri tem s predstavlja stranska plačila. Če obstaja maksimalni skupni sporazumni dobitek σ = max(x + y)  = max {x+y ; (x, y) ∈ S' } in je (u, v) pogajalsko izhodišče, je Nasheva rešitev igre kar (u, v), če je u+v > σ, sicer pa je to tista razdelitev maksimalnega skupnega
sporazumnega dobitka σ, pri kateri se ohrani razlika dobitkov iz pogajalskega
izhodišča. Torej 
<img src="https://render.githubusercontent.com/render/math?math=x^{\star} = \frac{\sigma + u - v}{2}, y^{\star} = \frac{\sigma - u + v}{2} ">

Pogodba ima sledeče sestavine: 

* Za vsak profil akcij $(a_1, a_2, \dots, a_n) \in A_1 \times A_2 \times \dots \times A_n$ določijo verjetnost s katero bodo ta profil igrali.

* Pri prenosljivih dobrinah imamo stranska plačia. 

Gledali bomo bimatrične igre, to so končne igre za dva igralca. Podana je s parom matrik (A , B), kjer je ${A, B} \subset \mathbf{R}^{m \times n}$. Pri tem so koristnosti prvega igralca podani kot elemani matrike A in koristnosti drugega igralca v matriki B. Igro predstavimo v matriki G, ki vseguje akcije igralcev. Koristnost prvega igralca je ravno nasprotna koristnosti drugega igralca.

## Opis projekta

Gledala bom bimatrične kooperativne igre. Kot prvo bom simulirala matriko akcij G, vrednosti bom vzela iz različnih porazdelitev. Omejila se bom na kvadratne matrike.

Gledala bom samo prenosljive dobrine, kar pomeni, da je rešitev sporazum. Kot prvo bom simulirala eno fazno igro, to pomeni da bom izhajala direktno iz matrike koristnosti, pri tem bom pri vsaki igri določila status quo. Nato pa še dvofazno igro, kjer izhajamo iz strateške igre za dva igralca. Pri tem imamo pri prenosljivih dobrinah preprosto $S = \{(x, y);   m \leq x + y \leq \sigma \},$ pri tem je $m$ minimum in $\sigma$ maksimum skupnih dobitkov. Dvofazna igra ima vedno Nashovo ravnovesje. 

Analizirala bom izplačila posameznika v igri pri različnih porazdelitvah koristnosti. Primerjala bom tudi razlike med eno in dvofazno Nashevo igro pogajanja.  
