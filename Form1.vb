
Public Class Form1

    Private celebration As New Celebration
    Private celebrationimage As New CelebrationImage
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'celebration.DayOfYear = celebration.DayOfYear
        celebration.Year = Now.Year
        celebration.DayOfYear = Now.DayOfYear
        celebrationimage.Year = Now.Year
        celebrationimage.DayOfYear = Now.DayOfYear
        LabelMobileCelebration.Text = celebration.MobileCelebration()
        LabelImmovableCelebration.Text = celebration.ImmovableCelebration()
        PictureBox1.Image = celebrationimage.AddImage
        Label1.Text = "Το Πάσχα του " + CStr(celebration.Year) + " είναι στης " + celebration.Easter(celebration.Year) + "."
    End Sub
    Private Sub DateTimePicker1_ValueChanged(sender As Object, e As EventArgs) Handles DateTimePicker1.ValueChanged
        celebration.Year = DateTimePicker1.Value.Year
        celebration.DayOfYear = DateTimePicker1.Value.DayOfYear
        LabelMobileCelebration.Text = celebration.MobileCelebration
        LabelImmovableCelebration.Text = celebration.ImmovableCelebration
        celebrationimage.DayOfYear = DateTimePicker1.Value.DayOfYear
        celebrationimage.Year = DateTimePicker1.Value.Year
        celebrationimage.DayOfYear = DateTimePicker1.Value.DayOfYear
        PictureBox1.Image = celebrationimage.AddImage
        Label1.Text = "Το Πάσχα του " + CStr(celebration.Year) + " είναι στης " + celebration.Easter(celebration.Year) + "."
    End Sub
End Class
