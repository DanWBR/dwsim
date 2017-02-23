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

namespace DWSIM.Thermodynamics.AdvancedEOS.EditingForms
{
    public partial class VPT_Editor : Form
    {

        bool Loaded = false;

        public VPTPropertyPackage PP;

        public VPT_Editor()
        {
            InitializeComponent();
        }

        private void VPT_Editor_Load(object sender, EventArgs e)
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
                                        PP.InteractionParameters[cp.Name].Add(cp2.Name, new VPT_IP());
                                        dgvkij.Rows.Add(new object[] {
								            cp.Name,
								            cp2.Name,
								            PP.InteractionParameters[cp.Name][cp2.Name].k1,
                                            PP.InteractionParameters[cp.Name][cp2.Name].k2,
                                            PP.InteractionParameters[cp.Name][cp2.Name].k3
							            });
                                    }
                                }
                            }
                            else
                            {
                                dgvkij.Rows.Add(new object[] {
						            cp.Name,
						            cp2.Name,
						            PP.InteractionParameters[cp.Name][cp2.Name].k1,
                                    PP.InteractionParameters[cp.Name][cp2.Name].k2,
                                    PP.InteractionParameters[cp.Name][cp2.Name].k3
					            });
                            }
                        }
                    }
                }
                else
                {
                    PP.InteractionParameters.Add(cp.Name, new Dictionary<string, VPT_IP>());
                    goto gt0;
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
                        double.TryParse(dgvkij.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out PP.InteractionParameters[id1][id2].k1);
                        break;
                    case 3:
                        double.TryParse(dgvkij.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out PP.InteractionParameters[id1][id2].k2);
                        break;
                    case 4:
                        double.TryParse(dgvkij.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out PP.InteractionParameters[id1][id2].k3);
                        break;
                }
            }


        }

    }
}
