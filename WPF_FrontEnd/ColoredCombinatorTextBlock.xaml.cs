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

        class ColorScheme
        {
            public string glyphs;
            public StringBuilder text = new StringBuilder();
            public TextBox textBlock;
        }

        List<ColorScheme> _colorSchemes = new List<ColorScheme>();
        ColorScheme _defaultScheme;
        ColorScheme _completeScheme;

        public void InitializeColorScheme()
        {
            Func<string, Brush, ColorScheme> makeColorScheme = (str, brush) =>
                {
                    var tb = new TextBox()
                    {
                        Background = Brushes.Transparent,
                        Foreground = brush,
                        FontFamily = new FontFamily("Consolas")
                    };
                    return new ColorScheme()
                    {
                        glyphs = str,
                        textBlock = tb
                    };
                };

            _colorSchemes.Add(makeColorScheme("()", Brushes.Gray));
            _colorSchemes.Add(makeColorScheme("S", Brushes.Green));
            _colorSchemes.Add(makeColorScheme("K", Brushes.Blue));
            _colorSchemes.Add(makeColorScheme("I", Brushes.Red));
            _defaultScheme = makeColorScheme("", Brushes.Black);
            _defaultScheme.textBlock.Background = Brushes.White;
            _completeScheme = makeColorScheme("", Brushes.Transparent);

            ContentSpace.Children.Add(_defaultScheme.textBlock);
            foreach (var scheme in _colorSchemes)
            {
                ContentSpace.Children.Add(scheme.textBlock);
            }
            ContentSpace.Children.Add(_completeScheme.textBlock);
        }

        private string _text;
        public string Text
        {
            get
            {
                return _text;
            }
            set
            {
                _text = value;

                // First clear all the text.
                foreach (var scheme in _colorSchemes)
                    scheme.text.Clear();
                _defaultScheme.text.Clear();
                _completeScheme.text.Clear();

                foreach (char c in _text)
                {
                    bool found = false;
                    foreach (var scheme in _colorSchemes)
                    {
                        if (scheme.glyphs.Contains(c))
                        {
                            scheme.text.Append(c);
                            found = true;
                        }
                        else if (Char.IsControl(c))
                        {
                            scheme.text.Append(c);
                        }
                        else
                        {
                            scheme.text.Append(' ');
                        }
                    }

                    if (!found)
                        _defaultScheme.text.Append(c);
                    else if (Char.IsControl(c))
                    {
                        _defaultScheme.text.Append(c);
                    }
                    else
                    {
                        _defaultScheme.text.Append(' ');
                    }
                }
                _completeScheme.text.Append(_text);

                foreach (var scheme in _colorSchemes)
                {
                    scheme.textBlock.Text = scheme.text.ToString();
                }
                _defaultScheme.textBlock.Text = _defaultScheme.text.ToString();
                _completeScheme.textBlock.Text = _completeScheme.text.ToString();
            }
        }
    }
}
