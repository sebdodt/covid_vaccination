# Parameter
Ansteckungsgefahr <- 100
Wartezeit_bis_BioNTech <- 14 #in Tagen
Wartezeit_bis_AstraZeneca <- 3 #in Tagen
R_Wert_aktuell <- 1.05
R_Wert_Mitte_Marz <- 1.7
R_Wert_Ende_Marz <- 0.5
R_Wert_Ende_April <- 0.9
R_Wert_danach <- 0.9

# AstraZeneca
# Quelle: https://www.thelancet.com/action/showPdf?pii=S0140-6736%2820%2932661-1
A_Schutz_1_Dosis <- 0.641
A_Schutz_2_Dosen <- 0.82 #(67.0 - 97.0)
A_Abstand_Dosen <- 84
Auffrischung_zu_BioNTech_oder_CoronaEnde <- 140

# BioNTech
# Quelle: https://www.nejm.org/doi/10.1056/NEJMoa2034577?url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org&rfr_dat=cr_pub++0pubmed
B_Schutz_1_Dosis <- 0.52 #(29.5 - 58.4)
B_Schutz_2_Dosen <- 0.91 
B_Schutz_2_Dosen_7_Tage <- 0.95 #(90.3 - 97.6)
B_Abstand_Dosen <- 21

#Graph
Tag <- seq(1,Auffrischung_zu_BioNTech_oder_CoronaEnde,1)+Sys.Date()
Tage_noch_im_Marz <- as.integer(as.Date("2021-04-01")-Sys.Date())
R_Wert <- rbind(matrix(seq(R_Wert_aktuell,R_Wert_Mitte_Marz,(R_Wert_Mitte_Marz-R_Wert_aktuell)/(Tage_noch_im_Marz/2))),
                matrix(seq(R_Wert_Mitte_Marz,R_Wert_Ende_Marz,(R_Wert_Ende_Marz-R_Wert_Mitte_Marz)/(Tage_noch_im_Marz/2))),
                matrix(seq(R_Wert_Ende_Marz,R_Wert_Ende_April,(R_Wert_Ende_April-R_Wert_Ende_Marz)/30)),
                matrix(seq(R_Wert_Ende_April,R_Wert_danach,(R_Wert_danach-R_Wert_Ende_April)/30)),
                matrix(rep(R_Wert_danach,Auffrischung_zu_BioNTech_oder_CoronaEnde-30-Tage_noch_im_Marz),ncol=1))
R_Wert_taeglich <- R_Wert^(1/10)

ungeimpft_Risiko <- vector(length = Auffrischung_zu_BioNTech_oder_CoronaEnde)
ungeimpft_Risiko[1] <- Ansteckungsgefahr * R_Wert_taeglich[1]
for (i in 2:Auffrischung_zu_BioNTech_oder_CoronaEnde) {
  ungeimpft_Risiko[i] <- ungeimpft_Risiko[i-1] * R_Wert_taeglich[i]
}

AstraZeneca_Risiko <- vector(length = Auffrischung_zu_BioNTech_oder_CoronaEnde)
if (Wartezeit_bis_AstraZeneca < 1) AstraZeneca_Risiko[1] <- Ansteckungsgefahr * R_Wert_taeglich[1] * A_Schutz_1_Dosis
if (Wartezeit_bis_AstraZeneca < 1) AstraZeneca_Risiko[1] <- Ansteckungsgefahr * R_Wert_taeglich[1]
if (Wartezeit_bis_AstraZeneca > 1) {
  for (i in 1:Wartezeit_bis_AstraZeneca) {
    AstraZeneca_Risiko[i] <- ungeimpft_Risiko[i]
  }
}
if (Wartezeit_bis_AstraZeneca < 1) Wartezeit_bis_AstraZeneca <- 1
for (i in Wartezeit_bis_AstraZeneca:(Wartezeit_bis_AstraZeneca+A_Abstand_Dosen)) {
  AstraZeneca_Risiko[i] <- ungeimpft_Risiko[i] * (1-A_Schutz_1_Dosis)
}
for (i in (Wartezeit_bis_AstraZeneca+A_Abstand_Dosen):Auffrischung_zu_BioNTech_oder_CoronaEnde) {
  AstraZeneca_Risiko[i] <- ungeimpft_Risiko[i] * (1-A_Schutz_2_Dosen)
}

BioNTech_Risiko <- vector(length = Auffrischung_zu_BioNTech_oder_CoronaEnde)
if (Wartezeit_bis_BioNTech < 1) BioNTech_Risiko[1] <- Ansteckungsgefahr * R_Wert_taeglich[1] * B_Schutz_1_Dosis
if (Wartezeit_bis_BioNTech < 1) BioNTech_Risiko[1] <- Ansteckungsgefahr * R_Wert_taeglich[1]
if (Wartezeit_bis_BioNTech > 1) {
  for (i in 1:Wartezeit_bis_BioNTech) {
    BioNTech_Risiko[i] <- ungeimpft_Risiko[i]
  }
}
if (Wartezeit_bis_BioNTech < 1) Wartezeit_bis_BioNTech <- 1
for (i in Wartezeit_bis_BioNTech:(Wartezeit_bis_BioNTech+B_Abstand_Dosen)) {
  BioNTech_Risiko[i] <- ungeimpft_Risiko[i] * (1-B_Schutz_1_Dosis)
}
for (i in (Wartezeit_bis_BioNTech+B_Abstand_Dosen):(Wartezeit_bis_BioNTech+B_Abstand_Dosen+7)) {
  BioNTech_Risiko[i] <- ungeimpft_Risiko[i] * (1-B_Schutz_2_Dosen)
}
for (i in (Wartezeit_bis_BioNTech+B_Abstand_Dosen+7):Auffrischung_zu_BioNTech_oder_CoronaEnde) {
  BioNTech_Risiko[i] <- ungeimpft_Risiko[i] * (1-B_Schutz_2_Dosen_7_Tage)
}


alldata = data.frame(cbind(Tag, ungeimpft_Risiko, AstraZeneca_Risiko, BioNTech_Risiko))
library(ggplot2)
library(ggpubr)
normal <- ggplot(alldata, aes(x = as.Date(Tag,origin = "1970-01-01"), y = ungeimpft_Risiko, colour = "ungeimpft")) +
  geom_line(stat = "identity") + scale_x_date(date_labels = "%B",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  geom_line(aes(y = AstraZeneca_Risiko, colour = "AstraZeneca")) +
  geom_line(aes(y = BioNTech_Risiko, colour = "BioNTech")) +
  ylim(0,max(ungeimpft_Risiko)) +
  xlab("") +
  ylab("Risiko (heute = 100)") +
  geom_vline(xintercept = Tag[Wartezeit_bis_AstraZeneca-1], linetype="dotted", colour = "red4") +
  geom_vline(xintercept = Tag[Wartezeit_bis_AstraZeneca+A_Abstand_Dosen-1], linetype="dotted", colour = "red4") +
  geom_vline(xintercept = Tag[Wartezeit_bis_BioNTech-1], linetype="dotted", colour = "springgreen4") +
  geom_vline(xintercept = Tag[Wartezeit_bis_BioNTech+B_Abstand_Dosen-1], linetype="dotted", colour = "springgreen4")

sum(ungeimpft_Risiko)
sum(BioNTech_Risiko)
sum(AstraZeneca_Risiko)

kumuliert_ungeimpft <- vector(length = Auffrischung_zu_BioNTech_oder_CoronaEnde)
kumuliert_AstraZeneca <- kumuliert_ungeimpft
kumuliert_BioNTech <- kumuliert_ungeimpft
for (i in 1:Auffrischung_zu_BioNTech_oder_CoronaEnde) {
  kumuliert_ungeimpft[i] <- sum(ungeimpft_Risiko[1:i])
  kumuliert_AstraZeneca[i] <- sum(AstraZeneca_Risiko[1:i])/kumuliert_ungeimpft[i] 
  kumuliert_BioNTech[i] <- sum(BioNTech_Risiko[1:i])/kumuliert_ungeimpft[i]
}
kumuliert_data <- data.frame(cbind(Tag, kumuliert_AstraZeneca, kumuliert_BioNTech))
kumuliert <- ggplot(kumuliert_data, aes(x = as.Date(Tag,origin = "1970-01-01"), y = 1, colour = "ungeimpft")) +
  geom_line(stat = "identity") + scale_x_date(date_labels = "%B",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  geom_line(aes(y = kumuliert_AstraZeneca, colour = "AstraZeneca")) +
  geom_line(aes(y = kumuliert_BioNTech, colour = "BioNTech")) +
  xlab("") +
  ylab("kumuliertes Risiko") +
  scale_y_continuous(labels = scales::percent) +  
  geom_vline(xintercept = Tag[Wartezeit_bis_AstraZeneca-1], linetype="dotted", colour = "red4") +
  geom_vline(xintercept = Tag[Wartezeit_bis_AstraZeneca+A_Abstand_Dosen], linetype="dotted", colour = "red4") +
  geom_vline(xintercept = Tag[Wartezeit_bis_BioNTech-1], linetype="dotted", colour = "springgreen4") +
  geom_vline(xintercept = Tag[Wartezeit_bis_BioNTech+B_Abstand_Dosen], linetype="dotted", colour = "springgreen4")

ggarrange(normal, kumuliert, nrow = 2)
