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
using System.Diagnostics;
using System.IO;

namespace WPF_FrontEnd
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            Output.InitializeColorScheme();
        }

        private void CurrentTerm_TextChanged(object sender, TextChangedEventArgs e)
        {
            var textBox = sender as TextBox;

            var psi = new ProcessStartInfo
                {
                    FileName = @"C:\Code\Haskell\CombinatoryLogic\Combinator.exe",
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    Arguments = textBox.Text,
                    CreateNoWindow = true,
                };

            using (var process = Process.Start(psi))
            {
                using (var reader = process.StandardOutput)
                {
                    Output.Text = reader.ReadToEnd();
                }
            }
        }
    }
}
