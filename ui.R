 
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rsconnect)
source("get_graphs.R")

setAccountInfo(name='zegui7',
               token='2F0FC13B25CB72A5227F3353E86ABD81',
               secret='zlzC6c+BIm04skM92oAqLaONRrw6mZRGoUXp1KP0')

shinyUI(fluidPage(
 titlePanel("SpotOnDB: A global assessment of protein-protein interfaces and hot-spots using the PPI4DOCK database",
            windowTitle = "SpotOnDB"),
 br(),br(),br(),
 
 tabsetPanel(
   tabPanel(h4("HS and interface"),graph_hsns,
            h5(strong("Hot-spots and interface")),
            helpText("Hot-spots (HS) are important residues of the protein-protein interface.",
                     "These are residues which, upon mutation, have a highly negative energetic",
                     "impact on the complex formation energy. This plot shows how amino acid",
                     "residues and HS are distributed in the protein-protein interface. The",
                     "enrichment factor (E. factor) is calculated by dividing the residues'",
                     "relative distribution as a HS by the average distribution of all amino",
                     "acids as HS.")),
   tabPanel(h4("SASA"),graph_SASA,
            h5(strong("Solvent-accessible surface area")),
            helpText("Solvent-accessible surface area (SASA) values measure how much",
                     "an amino acid is accessible to the solvent. Essentially, it calculates",
                     "how occluded an amino acid residue is to water using their Van der Waals", 
                     "radius.",
                     p("The values presented here were calculated using Visual Molecular",
                       "Dynamics. Values for unbound monomer (Monomer) and upon complex formation",
                       "(Complex) were obtained. Further SASA values were calculated, namely the", 
                       "difference between the Monomer and Complex SASA values (Delta) and the",
                       "ratio between the Delta SASA value and the Monomer SASA value (Relative)."))),
   tabPanel(h4("Structural features"),graph_struc,
            h5(strong("Structural features")),
            helpText("Structural features were all calculated using Visual Molecular Dynamics.",
                     "These include the number of atoms at 2.5Å (# of atoms at 2.5A) and 4.0Å",
                     "(# of atoms at 2.5A), hydrogen bonds (H-bonds), salt bridges (Salt bridges)",
                     "and hydrophobic interactions (Hydrophobic).")),
   tabPanel(h4("B-factor"),graph_bfact,
            h5(strong("B-factor")),
            helpText("The b-factor - also known as the temperature factor - measures how motile",
                     "an atom or group of atoms is. These values were retrieved from the PDB",
                     "files in the PPI4DOCK dataset. Three different values were considered:",
                     "b-factor for the full residue, for backbone atoms only and sidechains only."))
   ),
 br(),
 h4(strong("Cite us:")),
 p("SPOTON PAPER")
 )
)
