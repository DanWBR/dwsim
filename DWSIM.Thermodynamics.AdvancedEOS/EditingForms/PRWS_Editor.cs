using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using DWSIM.Interfaces;
using DWSIM.Thermodynamics.AdvancedEOS.Auxiliary;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;

namespace DWSIM.Thermodynamics.AdvancedEOS.EditingForms
{
    public partial class PRWS_Editor : Form
    {

        bool Loaded = false;

        public PRWSPropertyPackage PP;

        public PRWS_Editor()
        {
            Application.EnableVisualStyles();
            InitializeComponent();

        }

        private void PRWS_Editor_Load(object sender, EventArgs e)
        {

            Loaded = false;

            foreach (ICompoundConstantProperties cp in PP.Flowsheet.SelectedCompounds.Values)
            {
            gt0:
                if (PP.InteractionParameters.ContainsKey(cp.Name))
                {
                    foreach (ICompoundConstantProperties cp2 in PP.Flowsheet.SelectedCompounds.Values)
                    {
                        if (cp.Name != cp2.Name)
                        {
                            if (!PP.InteractionParameters[cp.Name].ContainsKey(cp2.Name))
                            {
                                //check if collection has id2 as primary id
                                if (PP.InteractionParameters.ContainsKey(cp2.Name))
                                {
                                    if (!PP.InteractionParameters[cp2.Name].ContainsKey(cp.Name))
                                    {
                                        PP.InteractionParameters[cp.Name].Add(cp2.Name, new PHSC_IP());
                                        double a12 = PP.InteractionParameters[cp.Name][cp2.Name].kij;
                                        dgvkij.Rows.Add(new object[] {
								            cp.Name,
								            cp2.Name,
								            a12
							            });
                                    }
                                }
                            }
                            else
                            {
                                double a12 = PP.InteractionParameters[cp.Name][cp2.Name].kij;
                                dgvkij.Rows.Add(new object[] {
						            cp.Name,
						            cp2.Name,
						            a12
					            });
                                dgvkij.Rows[dgvkij.Rows.Count - 1].Cells[0].Tag = cp.Name;
                                dgvkij.Rows[dgvkij.Rows.Count - 1].Cells[1].Tag = cp2.Name;
                            }
                        }
                    }
                }
                else
                {
                    PP.InteractionParameters.Add(cp.Name, new Dictionary<string, PHSC_IP>());
                    goto gt0;
                }

            }

            foreach (ICompoundConstantProperties cp in PP.Flowsheet.SelectedCompounds.Values)
            {
            gt2:
                if (PP.InteractionParametersNRTL.ContainsKey(cp.Name))
                {
                    foreach (ICompoundConstantProperties cp2 in PP.Flowsheet.SelectedCompounds.Values)
                    {
                        if (cp.Name != cp2.Name)
                        {
                            if (!PP.InteractionParametersNRTL[cp.Name].ContainsKey(cp2.Name))
                            {
                                //check if collection has id2 as primary id
                                if (PP.InteractionParametersNRTL.ContainsKey(cp2.Name))
                                {
                                    if (!PP.InteractionParametersNRTL[cp2.Name].ContainsKey(cp.Name))
                                    {
                                        PP.InteractionParametersNRTL[cp.Name].Add(cp2.Name, new NRTL_IPData());
                                        double a12 = PP.InteractionParametersNRTL[cp.Name][cp2.Name].A12;
                                        double a21 = PP.InteractionParametersNRTL[cp.Name][cp2.Name].A21;
                                        double alpha12 = PP.InteractionParametersNRTL[cp.Name][cp2.Name].alpha12;
                                        dgvnrtl.Rows.Add(new object[] {
								            cp.Name,
								            cp2.Name,
								            a12, a21, alpha12
							            });
                                    }
                                }
                            }
                            else
                            {
                                double a12 = PP.InteractionParametersNRTL[cp.Name][cp2.Name].A12;
                                double a21 = PP.InteractionParametersNRTL[cp.Name][cp2.Name].A21;
                                double alpha12 = PP.InteractionParametersNRTL[cp.Name][cp2.Name].alpha12;
                                dgvnrtl.Rows.Add(new object[] {
						            cp.Name,
						            cp2.Name,
						            a12, a21, alpha12
					            });
                                dgvnrtl.Rows[dgvnrtl.Rows.Count - 1].Cells[0].Tag = cp.Name;
                                dgvnrtl.Rows[dgvnrtl.Rows.Count - 1].Cells[1].Tag = cp2.Name;
                            }
                        }
                    }
                }
                else
                {
                    PP.InteractionParametersNRTL.Add(cp.Name, new Dictionary<string, NRTL_IPData>());
                    goto gt2;
                }

            }

            Loaded = true;

        }

        private void dgvkij_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {

            if (Loaded)
            {
                string id1 = dgvkij.Rows[e.RowIndex].Cells[0].Value.ToString();
                string id2 = dgvkij.Rows[e.RowIndex].Cells[1].Value.ToString();
                switch (e.ColumnIndex)
                {
                    case 2:
                        double.TryParse(dgvkij.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out PP.InteractionParameters[id1][id2].kij);
                        break;
                }
            }


        }

        private void dgvnrtl_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {

            if (Loaded)
            {
                string id1 = dgvnrtl.Rows[e.RowIndex].Cells[0].Value.ToString();
                string id2 = dgvnrtl.Rows[e.RowIndex].Cells[1].Value.ToString();
                switch (e.ColumnIndex)
                {
                    case 2:
                        double.TryParse(dgvnrtl.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out PP.InteractionParametersNRTL[id1][id2].A12);
                        break;
                    case 3:
                        double.TryParse(dgvnrtl.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out PP.InteractionParametersNRTL[id1][id2].A21);
                        break;
                    case 4:
                        double.TryParse(dgvnrtl.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out PP.InteractionParametersNRTL[id1][id2].alpha12);
                        break;
                }
            }


        }

    }
}
