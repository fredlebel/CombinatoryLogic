using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace WPF_FrontEnd
{
    /// <summary>
    /// Interaction logic for ColoredCombinatorTextBlock.xaml
    /// </summary>
    public partial class ColoredCombinatorTextBlock : UserControl
    {
        public ColoredCombinatorTextBlock()
        {
            InitializeComponent();
        }

        public string Text
        {
            get
            {
                return Complete_Box.Text;
            }
            set
            {
                Complete_Box.Text = value;
            }
        }

        public bool IsReadOnly
        {
            get
            {
                return Complete_Box.IsReadOnly;
            }
            set
            {
                Complete_Box.IsReadOnly = value;
            }
        }

        public event Action TextChanged;

        private void Complete_Box_TextChanged(object sender, TextChangedEventArgs e)
        {
            var defaultStrb = new StringBuilder(Complete_Box.Text.Length);
            var s_Strb = new StringBuilder(Complete_Box.Text.Length);
            var k_Strb = new StringBuilder(Complete_Box.Text.Length);
            var i_Strb = new StringBuilder(Complete_Box.Text.Length);
            var paran_Strb = new StringBuilder(Complete_Box.Text.Length);

            var rest_default = new StringBuilder[4] { s_Strb, k_Strb, i_Strb, paran_Strb };
            var rest_s = new StringBuilder[4] { defaultStrb, k_Strb, i_Strb, paran_Strb };
            var rest_k = new StringBuilder[4] { s_Strb, defaultStrb, i_Strb, paran_Strb };
            var rest_i = new StringBuilder[4] { s_Strb, k_Strb, defaultStrb, paran_Strb };
            var rest_paran = new StringBuilder[4] { s_Strb, k_Strb, i_Strb, defaultStrb };

            foreach (char c in Complete_Box.Text)
            {
                StringBuilder strb = null;
                StringBuilder[] rest = null;

                switch (c)
                {
                    case 'S':
                        strb = s_Strb;
                        rest = rest_s;
                        break;
                    case 'K':
                        strb = k_Strb;
                        rest = rest_k;
                        break;
                    case 'I':
                        strb = i_Strb;
                        rest = rest_i;
                        break;
                    case '(':
                    case ')':
                        strb = paran_Strb;
                        rest = rest_paran;
                        break;
                    default:
                        strb = defaultStrb;
                        rest = rest_default;
                        break;
                }

                // Add the character to the right string.
                strb.Append(c);
                // Update the rest with white space.
                foreach (var i in rest)
                {
                    if (Char.IsControl(c))
                        i.Append(c);
                    else
                        i.Append(' ');
                }
            }

            // Finally update the actual controls.
            Default_Box.Text = defaultStrb.ToString();
            S_Box.Text = s_Strb.ToString();
            K_Box.Text = k_Strb.ToString();
            I_Box.Text = i_Strb.ToString();
            Parantheses_Box.Text = paran_Strb.ToString();

            // Notify all listeners
            if (TextChanged != null)
                TextChanged();
        }
    }
}
