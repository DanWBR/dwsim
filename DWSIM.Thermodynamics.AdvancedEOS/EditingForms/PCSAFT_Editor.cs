using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using DWSIM.Interfaces;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;

namespace DWSIM.Thermodynamics.AdvancedEOS.EditingForms
{
    public partial class PCSAFT_Editor : Form
    {

        bool Loaded = false;

        public BaseSAFTPropertyPackage PP;

        public PCSAFT_Editor()
        {
            Application.EnableVisualStyles();
            InitializeComponent();
        }

        private void PCSAFT_Editor_Load(object sender, EventArgs e)
        {

            Loaded = false;

            chkUseLK.Checked = PP.UseLeeKeslerEnthalpy;

            List<ICompoundConstantProperties> compounds;
            
            if (GlobalSettings.Settings.CAPEOPENMode)
            {
                compounds = PP._selectedcomps.Values.Select(x => (ICompoundConstantProperties)x).ToList();            
            }
            else { 
                compounds = PP.Flowsheet.SelectedCompounds.Values.ToList();
            }

            foreach (ICompoundConstantProperties cp in compounds)
            {
            gt0:
                if (PP.InteractionParameters.ContainsKey(cp.CAS_Number))
                {
                    foreach (ICompoundConstantProperties cp2 in compounds)
                    {
                        if (cp.CAS_Number != cp2.CAS_Number)
                        {
                            if (!PP.InteractionParameters[cp.CAS_Number].ContainsKey(cp2.CAS_Number))
                            {
                                //check if collection has id2 as primary id
                                if (PP.InteractionParameters.ContainsKey(cp2.CAS_Number))
                                {
                                    if (!PP.InteractionParameters[cp2.CAS_Number].ContainsKey(cp.CAS_Number))
                                    {
                                        PP.InteractionParameters[cp.CAS_Number].Add(cp2.CAS_Number, new PCSIP());
                                        double a12 = PP.InteractionParameters[cp.CAS_Number][cp2.CAS_Number].kij;
                                        dgvkij.Rows.Add(new object[] {
								            cp.Name,
								            cp2.Name,
								            a12
							            });
                                        dgvkij.Rows[dgvkij.Rows.Count - 1].Cells[0].Tag = cp.CAS_Number;
                                        dgvkij.Rows[dgvkij.Rows.Count - 1].Cells[1].Tag = cp2.CAS_Number;
                                    }
                                }
                            }
                            else
                            {
                                double a12 = PP.InteractionParameters[cp.CAS_Number][cp2.CAS_Number].kij;
                                dgvkij.Rows.Add(new object[] {
						            cp.Name,
						            cp2.Name,
						            a12
					            });
                                dgvkij.Rows[dgvkij.Rows.Count - 1].Cells[0].Tag = cp.CAS_Number;
                                dgvkij.Rows[dgvkij.Rows.Count - 1].Cells[1].Tag = cp2.CAS_Number;
                            }
                        }
                    }
                }
                else
                {
                    PP.InteractionParameters.Add(cp.CAS_Number, new Dictionary<string, PCSIP>());
                    goto gt0;
                }
            }

            dgvparams.Rows.Clear();

            foreach (ICompoundConstantProperties cp in compounds)
            {
            gt1:
                if (PP.CompoundParameters.ContainsKey(cp.CAS_Number))
                {
                    double mw = PP.CompoundParameters[cp.CAS_Number].mw;
                    double m = PP.CompoundParameters[cp.CAS_Number].m;
                    double sigma = PP.CompoundParameters[cp.CAS_Number].sigma;
                    double epsilon = PP.CompoundParameters[cp.CAS_Number].epsilon;
                    string assocpar = PP.CompoundParameters[cp.CAS_Number].associationparams;
                    dgvparams.Rows.Add(new object[] {
			                    cp.Name,
			                    cp.CAS_Number,
			                    mw,
			                    m,
			                    sigma,
			                    epsilon
		                    });
                    dgvparama.Rows.Add(new object[] {
			                    cp.Name,
			                    assocpar
		                    });
                    dgvparama.Rows[dgvparama.Rows.Count - 1].Cells[0].Tag = cp.CAS_Number;
                }
                else
                {
                    PP.CompoundParameters.Add(cp.CAS_Number, new PCSParam());
                    goto gt1;
                }
            }

            Loaded = true;

        }

        private void dgvkij_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {

            if (Loaded)
            {
                string id1 = dgvkij.Rows[e.RowIndex].Cells[0].Tag.ToString();
                string id2 = dgvkij.Rows[e.RowIndex].Cells[1].Tag.ToString();
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
                string id = dgvparams.Rows[e.RowIndex].Cells[1].Value.ToString();
                switch (e.ColumnIndex)
                {
                    case 3:
                        PP.CompoundParameters[id].m = value;
                        break;
                    case 4:
                        PP.CompoundParameters[id].sigma = value;
                        break;
                    case 5:
                        PP.CompoundParameters[id].epsilon = value;
                        break;
                }
            }
        }

        private void dgvparama_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {
            if (Loaded)
            {
                string value = dgvparama.Rows[e.RowIndex].Cells[e.ColumnIndex].Value.ToString();
                string id = dgvparama.Rows[e.RowIndex].Cells[0].Tag.ToString();
                switch (e.ColumnIndex)
                {
                    case 1:
                        PP.CompoundParameters[id].associationparams = value;
                        break;
                }
            }
        }

        private void chkUseLK_CheckedChanged(object sender, EventArgs e)
        {
            PP.UseLeeKeslerEnthalpy = chkUseLK.Checked;
        }

    }
}
