using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Thermodynamics.Streams;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.SharedClasses.SystemsOfUnits;

using System.Threading.Tasks;

using Eto.Forms;
using Eto.Drawing;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;

namespace DWSIM.UI.Desktop.Editors.Utilities
{
    public class TrueCriticalPointView : DynamicLayout
    {

        public IFlowsheet flowsheet;
        
        public TrueCriticalPointView(IFlowsheet fs): base()
        {
            flowsheet = fs;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            var mslist = flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream).Select((x2) => x2.GraphicObject.Tag).ToList();

            mslist.Insert(0, "");

            this.CreateAndAddLabelRow("Setup");

            this.CreateAndAddDescriptionRow("The True Critical Point utility calculates the True Critical Point of a mixture using the Peng-Robinson or Soave-Redlich-Kwong EOS.");

            var spinner = this.CreateAndAddDropDownRow("Material Stream", mslist, 0, (arg3, arg2) => { });

            var button = this.CreateAndAddButtonRow("Calculate", null, (arg3, arg2) => { });

            this.CreateAndAddLabelRow("Results");

            var txtResults = this.CreateAndAddMultilineMonoSpaceTextBoxRow("", 150, true, null);

            button.Click += (sender, e) =>
            {

                if (spinner.SelectedIndex > 0)
                {

                    var ms = (MaterialStream)flowsheet.GetFlowsheetSimulationObject(mslist[spinner.SelectedIndex]);
                    var calc = new DWSIM.Thermodynamics.ShortcutUtilities.Calculation(ms);
                    calc.CalcType = DWSIM.Thermodynamics.ShortcutUtilities.CalculationType.CriticalPoint;

                    DWSIM.Thermodynamics.ShortcutUtilities.CalculationResults results = null;

                    Task.Factory.StartNew(() =>
                    {
                        results = calc.Calculate();
                    }).ContinueWith((t) =>
                    {
                        Application.Instance.Invoke(() =>
                        {
                            if (results.ExceptionResult == null)
                            {
                                txtResults.Text = results.TextOutput;
                            }
                            else
                            {
                                txtResults.Text = results.ExceptionResult.Message;
                            }
                        });
                    });

                }

            };

        }

    }
}