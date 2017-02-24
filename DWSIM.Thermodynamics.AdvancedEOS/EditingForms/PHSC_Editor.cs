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
    public partial class PHSC_Editor : Form
    {

        bool Loaded = false;

        public PHSCPropertyPackage PP;

        public PHSC_Editor()
        {
            Application.EnableVisualStyles();
            InitializeComponent();
        }


        private void PHSC_Editor_Load(object sender, EventArgs e)
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

            dgvparams.Rows.Clear();

            foreach (ICompoundConstantProperties cp in PP.Flowsheet.SelectedCompounds.Values)
            {
            gt1:
                if (PP.CompoundParameters.ContainsKey(cp.Name))
                {
                    double V = PP.CompoundParameters[cp.Name].V;
                    double A = PP.CompoundParameters[cp.Name].A;
                    double E = PP.CompoundParameters[cp.Name].E;
                    dgvparams.Rows.Add(new object[] {
			                    cp.Name,
			                    cp.CAS_Number,
			                    V,
			                    A,
			                    E
		                    });
                }
                else
                {
                    PP.CompoundParameters.Add(cp.Name, new PHSC_Param());
                    goto gt1;
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

        private void dgvparams_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {

            if (Loaded)
            {
                double value;
                double.TryParse(dgvparams.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString(), out value);
                string id = dgvparams.Rows[e.RowIndex].Cells[0].Value.ToString();
                switch (e.ColumnIndex)
                {
                    case 2:
                        PP.CompoundParameters[id].V = value;
                        break;
                    case 3:
                        PP.CompoundParameters[id].A = value;
                        break;
                    case 4:
                        PP.CompoundParameters[id].E = value;
                        break;
                }
            }
        }

    }
}
