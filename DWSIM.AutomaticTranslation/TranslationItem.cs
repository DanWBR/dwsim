using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.AutomaticTranslation
{
    public class TranslationItem
    {
        public string Text { get; set; } = "";
    }

    public class Translatables
    {
        public List<TranslationItem> Collection { get; set; } = new List<TranslationItem>();
    }

    public class TranslatedText
    {

        public string OriginalText { get; set; } = "";

        public Dictionary<string, string> Translations { get; set; } = new Dictionary<string, string>();
    
    }

    public class DetectedLanguage
    {
        public string language { get; set; } = "";
        public double score { get; set; } = 0.0;
    }

    public class Translation
    {
        public string text { get; set; } = "";
        public string to { get; set; } = "";
    }

    public class TranslationResult
    {
        public DetectedLanguage detectedLanguage { get; set; } = new DetectedLanguage();
        public List<Translation> translations { get; set; } = new List<Translation>();
    }

}
