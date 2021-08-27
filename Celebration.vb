Public Class Celebration
    Public Year As Integer = Now.Year
    Public DayOfYear As Integer = Now.DayOfYear
    Public Function RemainDaysOfYear()
        'Υπολογισμός δίσεκτου έτους (χρησιμοποιείτε για τον υπολογισμό της ημέρας το έτους)'
        'παίρνουμε το υπόλοιπο της διαιρέσεις του έτους  με το 4'
        Dim February As Integer
        Dim IntLeapYear As Integer = Year Mod 4
        If IntLeapYear = 0 Then
            February = 1
        Else
            February = 0
        End If
        Dim DayOfYear As Integer = Now.Day
        Return (365 + February) - DayOfYear 'fix it'
    End Function
    Public Function NameMonths(IntMonth As Integer)
        Dim NameMonth(12) As String
        NameMonth(0) = "NULL"
        NameMonth(1) = "Ιανουαρίου"
        NameMonth(2) = "Φεβρουαρίου"
        NameMonth(3) = "Ματρίου"
        NameMonth(4) = "Απριλίου"
        NameMonth(5) = "Μαΐου"
        NameMonth(6) = "Ιουνίου"
        NameMonth(7) = "Ιουλίου"
        NameMonth(8) = "Αυγούστου"
        NameMonth(9) = "Σεπτεμβρίου"
        NameMonth(10) = "Οκτωβρίου"
        NameMonth(11) = "Νοεμβρίου"
        NameMonth(12) = "Δεκεμβρίου"
        Return NameMonth(IntMonth)
    End Function
    Public Function Easter(year As Integer)
        'υπολογισμός του πάσχα'
        Dim y2 = Math.Floor(year / 100)
        Dim e = 10 + y2 - 16 - Math.Floor((y2 - 16) / 4)
        Dim G = year Mod 19
        Dim I = (19 * G + 15) Mod 30
        Dim J = (year + Math.Floor(year / 4) + I) Mod 7
        Dim L = I - J
        Dim p = L + e
        Dim d = 1 + (p + 27 + Math.Floor((p + 6) / 40)) Mod 31 'Ημερομηνία του Πάσχα'
        Dim m = 3 + Math.Floor((p + 26) / 30) 'Μηνας του Πάχα'
        Return CStr(d) + " " + NameMonths(m)
    End Function
    Public Function MobileCelebration()
        'Υπολογισμός δίσεκτου έτους (χρησιμοποιείτε για τον υπολογισμό της ημέρας το έτους)'
        'παίρνουμε το υπόλοιπο της διαιρέσεις του έτους  με το 4'
        Dim February As Integer
        Dim IntLeapYear As Integer = Year Mod 4

        If IntLeapYear = 0 Then
            February = 1
        Else
            February = 0
        End If
        'υπολογισμός του πάσχα'
        Dim y2 = Math.Floor(Year / 100)
        Dim e = 10 + y2 - 16 - Math.Floor((y2 - 16) / 4)
        Dim G = Year Mod 19
        Dim I = (19 * G + 15) Mod 30
        Dim J = (Year + Math.Floor(Year / 4) + I) Mod 7
        Dim L = I - J
        Dim p = L + e
        Dim d = 1 + (p + 27 + Math.Floor((p + 6) / 40)) Mod 31 'Ημερομηνία του Πάσχα'
        Dim m = 3 + Math.Floor((p + 26) / 30) 'Μηνας του Πάχα'
        'Υπολογισμός μεταβλητών εορτών'
        'κινητές εορτές. μετατροπή τη ημ/νιας και μινα σε ημέρα'
        Dim EsterDay As Integer
        Dim Lenght As Integer
        Dim Total As Integer
        If m = 4 Then
            EsterDay = 120 + February
            Lenght = 30
        ElseIf m = 5 Then
            EsterDay = 151 + February
            Lenght = 31
        End If
        Total = EsterDay + d
        EsterDay = Total - Lenght
        'αφου μετατραπει σε ημερα αφερούμε ημερες για να βρούμε της κινιτες εορτες'
        Dim telonoyfariseoy As Integer = EsterDay - 70
        Dim asotoy As Integer = EsterDay - 63
        Dim tsiknopemti As Integer = EsterDay - 59
        Dim psixosavato As Integer = EsterDay - 57
        Dim apokreo As Integer = EsterDay - 56
        Dim tyrofagoy As Integer = EsterDay - 49
        Dim deytera As Integer = EsterDay - 48
        Dim axeretism As Integer = EsterDay - 44
        Dim theodoroy As Integer = EsterDay - 43
        Dim orthodoksias As Integer = EsterDay - 42
        Dim bxeretism As Integer = EsterDay - 37
        Dim bnistion As Integer = EsterDay - 35
        Dim gxeretism As Integer = EsterDay - 30
        Dim gnistion As Integer = EsterDay - 28
        Dim dxeretism As Integer = EsterDay - 23
        Dim dnistion As Integer = EsterDay - 21
        Dim mkanon As Integer = EsterDay - 18
        Dim akymnos As Integer = EsterDay - 16
        Dim enistion As Integer = EsterDay - 14
        Dim lazaroy As Integer = EsterDay - 8
        Dim vaion As Integer = EsterDay - 7
        Dim mdeytera As Integer = EsterDay - 6
        Dim mtrith As Integer = EsterDay - 5
        Dim mtetarth As Integer = EsterDay - 4
        Dim mpempth As Integer = EsterDay - 3
        Dim epitafios As Integer = EsterDay - 2
        Dim msavato As Integer = EsterDay - 1
        Dim ageo As Integer = 113 + February

        'Εάν του άγιου Γεωργίου πέφτει στις πένθιμες ημέρες του Πάσχα εορτάζετε μια ημέρα'
        'μετά διαφορετικά εορτάζετε κανονικά στις 23 Απριλίου'
        If ageo <= EsterDay Then ageo = EsterDay + 1

        Dim tridiaken As Integer = EsterDay + 2
        Dim zoodoxoypigis As Integer = EsterDay + 5
        Dim thomas As Integer = EsterDay + 7
        Dim miroforon As Integer = EsterDay + 14
        Dim paralitoy As Integer = EsterDay + 21
        Dim mespentikostis As Integer = EsterDay + 24
        Dim samaritidos As Integer = EsterDay + 28
        Dim tifloy As Integer = EsterDay + 35
        Dim analipseos As Integer = EsterDay + 39
        Dim agpateron As Integer = EsterDay + 42
        Dim psixsavaton As Integer = EsterDay + 48
        Dim pentikosti As Integer = EsterDay + 49
        Dim agpnevmatos As Integer = EsterDay + 50
        Dim agpanton As Integer = EsterDay + 56

        'Δημιουργία (βάσης δεδομένον) δομής απόφασης για της κινητές γιορτές.'
        Dim kiniti As String
        If DayOfYear = telonoyfariseoy Then
            kiniti = "+ ΤΕΛΩΝΟΥ & ΦΑΡΙΣΑΙΟΥ. Αρχή τριωδίου. "
        ElseIf DayOfYear = asotoy Then
            kiniti = "+ ΤΟΥ ΑΣΩΤΟΥ. "
        ElseIf DayOfYear = tsiknopemti Then
            kiniti = "+ ΤΣΙΚΝΟΠΕΜΠΤΗ. "
        ElseIf DayOfYear = psixosavato Then
            kiniti = "+ ΨΥΧΟΣΑΒΒΑΤΟ. "
        ElseIf DayOfYear = apokreo Then
            kiniti = "+ ΑΠΟΚΡΕΩ. "
        ElseIf DayOfYear = tyrofagoy Then
            kiniti = "+ΤΗΣ ΤΥΡΟΦΑΓΟΥ (Κατάλυση Τύρου). "
        ElseIf DayOfYear = deytera Then
            kiniti = "+ ΚΑΘΑΡΑ ΔΕΥΤΕΡΑ. Αρχή νηστείας (Νηστεία). "
        ElseIf DayOfYear = axeretism Then
            kiniti = "+ Α ΧΑΙΡΕΤΙΣΜΟΙ (Νηστεία). "
        ElseIf DayOfYear = theodoroy Then
            kiniti = "+ θΕΟΔΩΡΟΥ ΤΗΡΩΝΟΣ (Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = orthodoksias Then
            kiniti = "+ Α ΝΗΣΤΕΙΩΝ. Της ορθοδοξίας " + vbCrLf + "(Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = bxeretism Then
            kiniti = "+ Β ΧΑΙΡΕΤΙΣΜΟΙ (Νηστεία)."
        ElseIf DayOfYear = bnistion Then
            kiniti = "+ Β ΝΗΣΤΕΙΩΝ. Γρ. Παλαμά " + vbCrLf + "(Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = gxeretism Then
            kiniti = "+ Γ ΧΑΙΡΕΤΙΣΜΟΙ (Νηστεία), "
        ElseIf DayOfYear = gnistion Then
            kiniti = "+ Γ ΝΗΣΤΕΙΩΝ. ΤΗΣ ΣΤΑΥΡΟΠΡΟΣΚΥΝΗΣΕΩΣ " + vbCrLf + "(Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = dxeretism Then
            kiniti = "+ Δ ΧΑΙΡΕΤΙΣΜΟΙ(Νηστεία), "
        ElseIf DayOfYear = dnistion Then
            kiniti = "+ Δ ΝΗΣΤΕΙΩΝ. Ιωάννου της Κλίμακος " + vbCrLf + "(Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = mkanon Then
            kiniti = "+ Ο ΜΕΓΑΛΟΣ ΚΑΝΩΝ (Νηστεία)."
        ElseIf DayOfYear = akymnos Then
            kiniti = "+ Ο ΑΚΑΘΙΣΤΟΣ ΥΜΝΟΣ (Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = enistion Then
            kiniti = "+ Ε ΝΗΣΤΕΙΩΝ. Μαρίας Αιγυπτίας " + vbCrLf + "(Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = lazaroy Then
            kiniti = "+ ΑΝΑΣΤΑΣΙΣ ΤΟΥ ΛΑΖΑΡΟΥ (Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = vaion Then
            kiniti = "+ ΤΩΝ ΒΑΪΩΝ (Κατάλυση οίνου και ελαίου)."
        ElseIf DayOfYear = mdeytera Then
            kiniti = "+ ΜΕΓΑΛΗ ΔΕΥΤΕΡΑ. Ιωσήφ του παγκάλου (Νηστεία)."
        ElseIf DayOfYear = mtrith Then
            kiniti = "+ ΜΕΓΑΛΗ ΤΡΙΤΗ. Των 10 Παρθένων (Νηστεία)."
        ElseIf DayOfYear = mtetarth Then
            kiniti = "+ ΜΕΓΑΛΗ ΤΕΤΑΡΤΗ. Της αλειψάσης τον " + vbCrLf + "Κύριον μύρω (Νηστεία)."
        ElseIf DayOfYear = mpempth Then
            kiniti = "+ ΜΕΓΑΛΗ ΠΕΜΠΤΗ. Ο Μυστικός Δείπνος (Νηστεία)."
        ElseIf DayOfYear = epitafios Then
            kiniti = "+ ΜΕΓΑΛΗ ΠΑΡΑΣΚΕΥΗ. Η Σταύρωσης και ο  " + vbCrLf + "Επιτάφιος (Νηστεία)."
        ElseIf DayOfYear = msavato Then
            kiniti = "+ ΜΕΓΑ ΣΑΒΒΑΤΟΝ. Η εις Άδου κάθοδος του Κυρίου" + vbCrLf + "(Νηστεία)."
        ElseIf DayOfYear = EsterDay Then
            kiniti = "+ΤΟ ΑΓΙΟΝ ΠΑΣΧΑ. Η ΑΝΑΣΤΑΣΙΣ ΤΟΥ ΚΥΡΙΟΥ,"

        ElseIf DayOfYear = CInt(ageo) & CInt(ageo) = CInt(EsterDay) + 1 Then
            kiniti = "+Δευτέρα Διακαινησίμου, Γεωργίου του Τροπαιοφόρου."
        ElseIf DayOfYear = CInt(ageo) & CInt(ageo) = 113 + February Then
            kiniti = "+Γεωργίου του Τροπαιοφόρου, Λαζάρου Νεομάρτυρος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου)."

        ElseIf DayOfYear = CInt(EsterDay) + 1 & Int(ageo) = 113 + February Then
            kiniti = "+ Δευτέρα Διακαινησίμου."
        ElseIf DayOfYear = tridiaken Then
            kiniti = "+ Τρίτη Διακαινησίμου."
        ElseIf DayOfYear = zoodoxoypigis Then
            kiniti = "+ ΖΩΟΔΟΧΟΥ ΠΗΓΗΣ."
        ElseIf DayOfYear = thomas Then
            kiniti = "+ ΤΟΥ ΘΩΜΑ."
        ElseIf DayOfYear = miroforon Then
            kiniti = "+ ΤΩΝ ΜΥΡΟΦΟΡΩΝ."
        ElseIf DayOfYear = paralitoy Then
            kiniti = "+ ΤΟΥ ΠΑΡΑΛΥΤΟΥ."
        ElseIf DayOfYear = mespentikostis Then
            kiniti = "+ Της Μεσοπεντηκοστής (Κατάλυση Ιχθύος)."
        ElseIf DayOfYear = samaritidos Then
            kiniti = "+ ΤΗΣ ΣΑΜΑΡΕΙΤΙΔΟΣ."
        ElseIf DayOfYear = tifloy Then
            kiniti = "+ ΤΟΥ ΤΥΦΛΟΥ."
        ElseIf DayOfYear = analipseos Then
            kiniti = "+ ΤΗΣ ΑΝΑΛΗΨΕΩΣ."
        ElseIf DayOfYear = agpateron Then
            kiniti = "+ ΤΩΝ 318 ΑΓΙΩΝ ΠΑΤΕΡΩΝ."
        ElseIf DayOfYear = psixsavaton Then
            kiniti = "+ ΨΥΧΟΣΑΒΒΑΤΟ."
        ElseIf DayOfYear = pentikosti Then
            kiniti = "+ ΤΗΣ ΠΕΝΤΗΚΟΣΤΗΣ."
        ElseIf DayOfYear = agpnevmatos Then
            kiniti = "+ ΤΟΥ ΑΓΙΟΥ ΠΝΕΥΜΑΤΟΣ."
        ElseIf DayOfYear = agpanton Then
            kiniti = "+ ΤΩΝ ΑΓΙΩΝ ΠΑΝΤΩΝ."
        Else
            kiniti = ""
        End If
        Return kiniti
    End Function
    Public Function ImmovableCelebration()
        'Υπολογισμός δίσεκτου έτους (χρησιμοποιείτε για τον υπολογισμό της ημέρας το έτους)'
        'παίρνουμε το υπόλοιπο της διαιρέσεις του έτους  με το 4'
        Dim IntLeapYear As Integer = Year Mod 4

        'Δημιουργία (βάσης δεδομένον) δομής απόφασης για της σταθερές εορτές...'
        Dim ChurchFeast As String = ""
        Dim NameFeast As String = ""
        Dim WorldDay As String = ""
        Dim SunRise As String = ""
        If IntLeapYear = 0 Then
            If DayOfYear = 1 Then
                '1η Ιανουαρίου'
                ChurchFeast = "+ περιτομή Ιησού Χριστού,  " + vbCrLf + "Βασιλίου του Μέγα, "
                NameFeast = "γιορτάζουν οι: Βασίλειος, Βασιλική, " + vbCrLf + "Βιβή, Βιβιαν, Βίκυ,Τηλέμαχος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:16"
            ElseIf DayOfYear = 2 Then
                ChurchFeast = "Σιλβέστρου ρώμης, Θεογένους ιερομ. "
                NameFeast = "Γιορτάζουν οι: Θεόπεμπτος, Σίλβεστρος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:17"
            ElseIf DayOfYear = 3 Then
                ChurchFeast = "Μαλαχίου προφ, Γορδίου μάρτ. "
                NameFeast = "Γιορτάζει η Γενοβέφα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:18"

            ElseIf DayOfYear = 4 Then
                ChurchFeast = "Θεοκτίστου, Ουνουφίου εν Χίω. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:19"

            ElseIf DayOfYear = 5 Then
                ChurchFeast = "Θεοπέμπτου, Θεώνα, Συγκλητικίς οσίου, "
                NameFeast = "γιορτάζουν οι: Θεωνάς, Θεώνη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:42 Δύση Ήλιου 17:19"
            ElseIf DayOfYear = 6 Then
                ChurchFeast = "+ΤΑ ΑΓΙΑ ΘΕΟΦΑΝΕΙΑ, "
                NameFeast = "γιορτάζουν οι: Φώτης, Θεοφάνης, Φωτεινή, " + vbCrLf + "Θεοπούλα, Ιορδάνης, Ουρανία, Περιστέρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:42 Δύση Ήλιου 17:20"

            ElseIf DayOfYear = 7 Then
                ChurchFeast = "+Σύναξις Ιωαννου του Προδρόμου Βαπτιστού " + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν οι: Γιάννης, Γιάννα, Ζαννέτα, Ζανέτ, Πρόδρομος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:42 Δύση Ήλιου 17:21"

            ElseIf DayOfYear = 8 Then
                ChurchFeast = "Γεωργίου Χοζεβίτου-Δομνίκης-Κύρου, "
                NameFeast = "γιορτάζουν οι: Αγάθων, Δομνίκη, Κέλσιος,  " + vbCrLf + "Παρθένα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:22"

            ElseIf DayOfYear = 9 Then
                ChurchFeast = "Πολυεύκτου Μάρτυρος, Ευστρατίου οσίου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:23"

            ElseIf DayOfYear = 10 Then
                ChurchFeast = "+ Γρηγορ.Νύσσης, Δομετιανού, Μελιτινής, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:24"

            ElseIf DayOfYear = 11 Then
                ChurchFeast = "Θεοδοσίου, αγαπίου, Μιχαήλ Θαυματ. "
                NameFeast = "Γιορτάζει ο Θεοδόσης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:25"

            ElseIf DayOfYear = 12 Then
                ChurchFeast = "Τατιανής μαρ.,Μερτίου,Ευθασίας, Σάββα. "
                NameFeast = "γιορτάζει η Τατιάνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:26"

            ElseIf DayOfYear = 13 Then
                ChurchFeast = "Ερμύλου και Στρατονίκου, Παχομίου οσίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:27"

            ElseIf DayOfYear = 14 Then
                ChurchFeast = "Απόδ.Θεοφανείων, Εν σινά και Ραϊθό Πατέρ. "
                NameFeast = "Γιορτάζει η Νίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:28"

            ElseIf DayOfYear = 15 Then
                ChurchFeast = "Παύλου, Ιωάννου Καλυβίτου, Πονσουφίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:29"

            ElseIf DayOfYear = 16 Then
                ChurchFeast = "+Προσκ. Αλύσ. Απ.Πέτρου,Δαμασκηνού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:30"

            ElseIf DayOfYear = 17 Then
                ChurchFeast = "+ Αντωνίου Μεγ.-Γεωργίου Ιωανν. " + vbCrLf + "Αχιλλά, (Κατάλυσης οίνου και ελαίου) "
                NameFeast = "γιορτάζουν οι: Αντώνης, Αντωνία, " + vbCrLf + "Νάκος, Θεοδόσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:31"

            ElseIf DayOfYear = 18 Then
                ChurchFeast = "+ Αθανασίου και Κυρίλλου Αλεξανδρείας. " + vbCrLf + "(Κατάλυσης οίνου και ελαίου) "
                NameFeast = "γιορτάζουν οι: Αθανασία, Θανάσης, Νάνσυ, " + vbCrLf + "Κύριλλος, Θεόδουλος, Θεοδούλη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:32"

            ElseIf DayOfYear = 19 Then
                ChurchFeast = "Μακαρίου Αιγυπτίου, Μάρκου, Αρσενίου, "
                NameFeast = "γιορτάζει ο Μακάριος, Ευφρασία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:33"

            ElseIf DayOfYear = 20 Then
                ChurchFeast = "+ Ευθυμίου Μεγ.-Ζαχαρίου-Ευσεβίου,  " + vbCrLf + "(Κατάλυσης οίνου και ελαίου) "
                NameFeast = "γιορτάζουν οι: Ευθύμιος, Ευθυμία, Θέμης, " + vbCrLf + "Φαβαινός, Φαβαινή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:34"

            ElseIf DayOfYear = 21 Then
                ChurchFeast = "Μαξίμου Ομολ., Αγνής, Νεοφύτου, Ακύλα, "
                NameFeast = "γιορτάζουν οι: Αγνή, αγνούλα, Ευγένιος, " + vbCrLf + "Πάτροκλος, Μάξιμος, Νεόφυτος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:35"

            ElseIf DayOfYear = 22 Then
                ChurchFeast = "+Τιμοθέου Απ., Αναστ.Πέρσου, Ιωσήφ  " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Τιμόθεος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:37 Δύση Ήλιου 17:36"

            ElseIf DayOfYear = 23 Then
                ChurchFeast = "Κλήμεντος, Αγαθαγγέλου, Διονυσίου, "
                NameFeast = "γιορτάζουν οι: Αγαθάγγελος, Αγαθαγγέλα,  " + vbCrLf + "fΑγαθαγγέλη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:37 Δύση Ήλιου 17:37"

            ElseIf DayOfYear = 24 Then
                ChurchFeast = "Ξένης οσίας, Βαβύλα, Ζωσιμά, Φίλωνος, "
                NameFeast = "γιορτάζουν οι: Ζωσιμάς, Ζωσιμίνα, Ξένη, Ξένια, " + vbCrLf + "Φίλωνας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:36 Δύση Ήλιου 17:39"

            ElseIf DayOfYear = 25 Then
                ChurchFeast = "+Γρηγορίου Θεολόγου, Μαργαρίτας  " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Γρηγόρης, Γρηγορία, Μαργαρίτα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:40"

            ElseIf DayOfYear = 26 Then
                ChurchFeast = "Ξενοφώντος-Κλήμεντος-Συμεών-Μαρίας, "
                NameFeast = "γιορτάζει ο Ξενοφώντας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:41"

            ElseIf DayOfYear = 27 Then
                ChurchFeast = "+ Ανακ. Λειψ. Ιωάν. Χρυσοστ. Μαρκιανής " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Χρυσόστομος. "
                WorldDay = "Παγκόσμια Ημέρα κατά της Λέπρας, Διεθνής  " + vbCrLf + "Ημέρα Μνήμης για τα θύματα του Ολοκαυτώματος."
                SunRise = "Ανατολή Ήλιου 7:34 Δύση Ήλιου 17:42"

            ElseIf DayOfYear = 28 Then
                ChurchFeast = "Εφραίμ του σύρου, Χάριτος, Παλλαδίου, Ιακώβου, "
                NameFeast = "Σήμερα γιορτάζουν οι: Παλάδιος, Χάρης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:43"

            ElseIf DayOfYear = 29 Then
                ChurchFeast = "+ Ανακ. Λειψ. Ιγν. Θεοφόρ., Παρηγορίου, "
                NameFeast = "γιορτάζουν οι: Βαρσαμία, Βαρσάμω."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:44"

            ElseIf DayOfYear = 30 Then
                ChurchFeast = "+Τριών Ιεραρχών, Εύρ. Εικ. Ευαγγ. Τήνου  " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Μαύρος, Μαυρουδής, Χρυσή."
                WorldDay = "Σχολική εορτή"
                SunRise = "Ανατολή Ήλιου 7:32 Δύση Ήλιου 17:45"

            ElseIf DayOfYear = 31 Then
                ChurchFeast = "Κύρου και Ιωάννου Αν., Αρσενίου εν Πάρω, "
                NameFeast = "γιορτάζουν οι: Ευδοξία, Κύρος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:31 Δύση Ήλιου 17:46"

            ElseIf DayOfYear = 32 Then
                '1η Φεβρουάριου'
                ChurchFeast = "Τρύφωνος, Περπέτουας, Αναστασίου, "
                NameFeast = "γιορτάζουν οι: Τρύφωνας, Φιλικητάτη, Φιλικήτη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:30 Δύση Ήλιου 17:48"

            ElseIf DayOfYear = 33 Then
                ChurchFeast = "+ Η ΥΠΑΠΑΝΤΙ Ι. ΧΡΙΣΤΟΥ, Ιορδάνου  " + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζει η Υπαπαντή. "
                WorldDay = "Παγκόσμια Ημέρα Υγροτόπων."
                SunRise = "Ανατολή Ήλιου 7:29 Δύση Ήλιου 17:49"

            ElseIf DayOfYear = 34 Then
                ChurchFeast = "Συμεών Θεοδόχου, Άννης, Σταματίου, "
                NameFeast = "γιορτάζουν οι: Συμεών, Ασημάκης, Ασημίνα, " + vbCrLf + "Σταμάτης, Μαλαματή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:29 Δύση Ήλιου 17:50"

            ElseIf DayOfYear = 35 Then
                ChurchFeast = "Ισιδώρου. Πηλουσιώτου, Νικολάου ομολογητού, "
                NameFeast = "γιορτάζουν οι: Ιάσιμος, Ιασίμη, Σίμος, " + vbCrLf + "Ισίδωρος, Ισιδώρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:28 Δύση Ήλιου 17:51"

            ElseIf DayOfYear = 36 Then
                ChurchFeast = "Αγάθης, Θεοδούλης μάρτ., Πολυεύκτου, "
                NameFeast = "γιορτάζουν οι: Αγαθή, Αγαθούλα, Αγαθώ, " + vbCrLf + "Αγαθίτσα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:27 Δύση Ήλιου 17:52"

            ElseIf DayOfYear = 37 Then
                ChurchFeast = "+ Φωτίου κων/πόλεως, Βουκόλου, Ιωσήφ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:26 Δύση Ήλιου 17:53"

            ElseIf DayOfYear = 38 Then
                ChurchFeast = "Παρθενίου Λαμψάκου, Λουκά οσίου, "
                NameFeast = "γιορτάζει ο Παρθένιος. "
                WorldDay = "Παγκόσμια Ημέρα Ασφαλούς Πλοήγησης στο " + vbCrLf + "Διαδίκτυο."
                SunRise = "Ανατολή Ήλιου 7:25 Δύση Ήλιου 17:54"

            ElseIf DayOfYear = 39 Then
                ChurchFeast = "+Θεοδώρου Στρατηλάτου, Ζαχαρίου Προφήτου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ζαχαρίας, Ζαχαρούλα, Θεόδωρος," + vbCrLf + "Θεοδώρα, Μάρθα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:24 Δύση Ήλιου 17:55"

            ElseIf DayOfYear = 40 Then
                ChurchFeast = "Νικηφόρου μάρτ., Μαρκέλλου, Φιλαγρίου, "
                NameFeast = "γιορτάζουν οι: Νικηφόρος, Νικηφορία, Μαρκέλος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:23 Δύση Ήλιου 17:56"

            ElseIf DayOfYear = 41 Then
                '10 Φεβρουαρίου'
                ChurchFeast = "+Χαράλαμπος ιερομ, Ζήνωνος ταχυδρόμου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Χαράλαμπος, Χαρίλαος, Χαρούλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:22 Δύση Ήλιου 17:58"

            ElseIf DayOfYear = 42 Then
                ChurchFeast = "+Βλασίου ιερομ, Θεοδώρας Αθγούστης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Αυγή, Βλάσιος, Βλάσης, " + vbCrLf + "Θεοδώρα, Δώρα. "
                WorldDay = "Παγκόσμια Ημέρα Του Γάμου."
                SunRise = "Ανατολή Ήλιου 7:21 Δύση Ήλιου 17:59"

            ElseIf DayOfYear = 43 Then
                ChurchFeast = "Μελετίου Αντιοχείας, Αντωνίου Κωνσ/λεως, "
                NameFeast = "γιορτάζουν οι: Μελέτιος, Μελέτης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:19 Δύση Ήλιου 18:00"

            ElseIf DayOfYear = 44 Then
                ChurchFeast = "Ακύλα, Πρισκίλλης αποστ., Μαρτινιανού, "
                NameFeast = "γιορτάζουν: Ακύλας, Πρίσκιλλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:18 Δύση Ήλιου 18:01"

            ElseIf DayOfYear = 45 Then
                ChurchFeast = "Αυξεντίου, Μάρωνος, Γεωργίου Μυτιλήνης, "
                NameFeast = "γιορτάζουν οι: Λουκάς, Βαλεντίνος, Βαλεντίνη. "
                WorldDay = "Ημέρα των ερωτευμένων Καθολικών Χριστιανών."
                SunRise = "Ανατολή Ήλιου 7:17 Δύση Ήλιου 18:02"

            ElseIf DayOfYear = 46 Then
                ChurchFeast = "Ονησίμου αποστόλου, Ευσεβίου, Ανθίμου, "
                NameFeast = "γιορτάζουν οι: Ευσέβιος, Ευσεβής, Ευσεβεία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:16 Δύση Ήλιου 18:03"

            ElseIf DayOfYear = 47 Then
                ChurchFeast = "Ουάλεντος, Φλαβιανού, Παμφίλου μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:15 Δύση Ήλιου 18:04"

            ElseIf DayOfYear = 48 Then
                ChurchFeast = "+Θεοδώρου Τύρωνος, Πουλχερίας, Μαρκιανού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:14 Δύση Ήλιου 18:05"

            ElseIf DayOfYear = 49 Then
                ChurchFeast = "Λέοντος Ρώμης, Αγαπητού Επισκόπου Σιναίου, "
                NameFeast = "γιορτάζουν οι: Αγαπητός, Λέων."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:12 Δύση Ήλιου 18:06"

            ElseIf DayOfYear = 50 Then
                ChurchFeast = "Φιλοθέης Αθηναίας, Φιλήμονος, Κόνωνος οσίου, "
                NameFeast = "γιορτάζουν οι: φιλοθέη, Χλόη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:11 Δύση Ήλιου 18:07"

            ElseIf DayOfYear = 51 Then
                '20 Φεβρουαρίου'
                ChurchFeast = "Λέοντος Κατάνης, Αγάθωνος Ρώμης, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:10 Δύση Ήλιου 18:08"

            ElseIf DayOfYear = 52 Then
                ChurchFeast = "Τιμοθέου εν Συμβ., Ευσταθίου Αντιοχείας, "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Μητρικής Γλώσσας, Παγκόσμια " + vbCrLf + "Ημέρα Του Ξεναγού."
                SunRise = "Ανατολή Ήλιου 7:09 Δύση Ήλιου 18:10"

            ElseIf DayOfYear = 53 Then
                ChurchFeast = "Των εν τοις Ευγενίου μαρτ. Θαλασσίου, "
                NameFeast = "γιορτάζουν οι: Ανθούσα, Ανθούση, Θαλάσσιος, Θάλασσα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:07 Δύση Ήλιου 18:11"

            ElseIf DayOfYear = 54 Then
                ChurchFeast = "Πολυκάρπου Σμύρνης, Προτερίου ιερομάρτυρος, "
                NameFeast = "γιορτάζει ο Πολύκαρπος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:06 Δύση Ήλιου 18:12"

            ElseIf DayOfYear = 55 Then
                ChurchFeast = " +Α' και Β' εύρεσης Τιμίας κεφαλής Προδρόμου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:05 Δύση Ήλιου 18:13"

            ElseIf DayOfYear = 56 Then
                ChurchFeast = "Ταρασίου Κων/λεως, Ρηγίνου Σκοπέλου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:03 Δύση Ήλιου 18:13"

            ElseIf DayOfYear = 57 Then
                ChurchFeast = "Φωτεινής Σαμαρείτιδος, Πορφυρίου Γάζης, "
                NameFeast = "γιορτάζουν οι: Ανατολή, Φωτεινή, Πορφύριος, " + vbCrLf + "Σεβαστιανός."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:03 Δύση Ήλιου 18:14"

            ElseIf DayOfYear = 58 Then
                ChurchFeast = "Προκοπίου Δακαπολίτου, Στεφάνου, "
                NameFeast = "γιορτάζουν οι: Ασκληπιός, Νήσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:02 Δύση Ήλιου 18:15"

            ElseIf DayOfYear = 59 Then
                ChurchFeast = "Βασιλείου ομολ.-Κυράννης, Κασσιανού, "
                NameFeast = "γιορτάζει η Μαριάννα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:01 Δύση Ήλιου 18:16"

            ElseIf DayOfYear = 60 Then
                '29 Φεβρουαρίου'
                ChurchFeast = "Κασιανού οσίου και ομολογιτού του Ρωμαίου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:59 Δύση Ήλιου 18:17"

            ElseIf DayOfYear = 61 Then
                '1η Μαρτίου'
                ChurchFeast = "Ευδοκίας, Αντωνίνης, Μαρκέλου μαρτ. "
                NameFeast = " Γιορτάζουν οι: Ευδοκία, Παρασκευάς."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:58 Δύση Ήλιου 18:18"

            ElseIf DayOfYear = 62 Then
                ChurchFeast = "Νικολάου Πλανά Ναξίου, Ησυχίου, Ευθαλίας, "
                NameFeast = "γιορτάζει η Ευθαλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:55 Δύση Ήλιου 18:20"

            ElseIf DayOfYear = 63 Then
                ChurchFeast = "Ευτροπίου, Κλεονίκου, Βασιλίσκου μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:54 Δύση Ήλιου 18:21"

            ElseIf DayOfYear = 64 Then
                ChurchFeast = "Γερασίμου του εν ΙΖορδάνη, Παύλου και " + vbCrLf + "Ιουλιανής μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:52 Δύση Ήλιου 18:22"

            ElseIf DayOfYear = 65 Then
                ChurchFeast = "Κόνωνος οσίου, Αρχελάου, Ευλαμπίου, "
                NameFeast = "γιορτάζουν οι: Αρχέλαος, Ευλόγιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:51 Δύση Ήλιου 18:23"

            ElseIf DayOfYear = 66 Then
                ChurchFeast = "Εύρ. Τιμίου Σταυρού, Εν Αμορίω 42 μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:49 Δύση Ήλιου 18:24"

            ElseIf DayOfYear = 67 Then
                ChurchFeast = "Λαυρεντίου οσίου, Βασιλέως και Αιθερίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:48 Δύση Ήλιου 18:25"

            ElseIf DayOfYear = 68 Then
                ChurchFeast = "Θεοφυλάκτου Νικομ.-Ερμού, Δομετίου, "
                NameFeast = "γιορτάζει ο Ερμής. "
                WorldDay = "Διεθνής Ημέρα της Γυνέκας."
                SunRise = "Ανατολή Ήλιου 6:47 Δύση Ήλιου 18:26"

            ElseIf DayOfYear = 69 Then
                ChurchFeast = "+Των έν Σεβαστεία Αγ. 40 Μαρτύρων, Καισαρίου " + vbCrLf + "ιατρού (Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βιβιάνα, Ηλιανός, Ηλιάνα, " + vbCrLf + "Λυσίμαχος, Σμαράγδα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:45 Δύση Ήλιου 18:27"

            ElseIf DayOfYear = 70 Then
                '10 Μαρτίου'
                ChurchFeast = "Κορδάτου Κορίνθου, Αναστασίας Πατρικίας, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:44 Δύση Ήλιου 18:28"

            ElseIf DayOfYear = 71 Then
                ChurchFeast = "Σωφρονίου Ιεροσαλ., Θαλλού, Θεοδώρας Άρτης, "
                NameFeast = "γιορτάζει ο Θαλλής. "
                WorldDay = "Ημέρα Των Θυμάτων Της Τρομοκρατίας."
                SunRise = "Ανατολή Ήλιου 6:42 Δύση Ήλιου 18:29"

            ElseIf DayOfYear = 72 Then
                ChurchFeast = "Συμεών Θεολόγου, Θεοφάνους ομολ., Γρηγορίου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:41 Δύση Ήλιου 18:30"

            ElseIf DayOfYear = 73 Then
                ChurchFeast = "Νικηφόρου Κωνστ/λεως, Πουπλίου Αθηνών, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:39 Δύση Ήλιου 18:31"

            ElseIf DayOfYear = 74 Then
                ChurchFeast = "Βενεδίκτου οσίου, Ευσχήμονος ομολογ. "
                NameFeast = "Γιορτάζουν οι: Βενέδικτος, Βενεδίκτη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:38 Δύση Ήλιου 18:32"

            ElseIf DayOfYear = 75 Then
                ChurchFeast = "Αγαπίου, Αριστοβούλου, Τιμολάου, "
                NameFeast = "γιορτάζει ο Αγάπιος. "
                WorldDay = "Παγκόσμια Ημέρα Καταναλωτή, Παγκόσμια Ημέρα " + vbCrLf + "Κατά της Αστυνομικής Βαρβαρότητας."
                SunRise = "Ανατολή Ήλιου 6:36 Δύση Ήλιου 18:33"

            ElseIf DayOfYear = 76 Then
                ChurchFeast = "Χριστοδούλου Πάτμου, Σαββίνου, "
                NameFeast = "γιορτάζουν οι: Ιουλιανός, Χριστόδουλος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:35 Δύση Ήλιου 18:33"

            ElseIf DayOfYear = 77 Then
                ChurchFeast = "+ Αλεξίου Ανθρώπου Θεού, Θεοστηρίκτου, "
                NameFeast = "γιορτάζουν οι: Αλέξιος, Αλεξία, Αλέξής, Αλέκος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:33 Δύση Ήλιου 18:34"

            ElseIf DayOfYear = 78 Then
                ChurchFeast = "Κυρίλλου, Τροφίμου, Ευκαρπίωνος, "
                NameFeast = ""
                WorldDay = " Διεθνής Ημέρα κατά της Κατοχής του Ιράκ."
                SunRise = "Ανατολή Ήλιου 6:32 Δύση Ήλιου 18:35"

            ElseIf DayOfYear = 79 Then
                ChurchFeast = "Χρυσάνθου και Δαρείας, Κλαυδίου μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Ιωσήφ, Δαρεία, Χρύσανθος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:30 Δύση Ήλιου 18:36"

            ElseIf DayOfYear = 80 Then
                '20 Μαρτίου'
                ChurchFeast = "Εν μονή Αγ. Σάββα αναιρθέντων, Μύρωνος, "
                NameFeast = "γιορτάζει η Ροδή. "
                WorldDay = "Διεθνής Ημέρα της γής, Παγκόσμια Ημέρα Αποχής " + vbCrLf + "από το Κρέας, Διεθνής Ημέρα Αστρολογίας, " + vbCrLf + "Παγκοσμια Ημέρα Θεάτρου για τα Παιδιά " + vbCrLf + "Και τους Νέους, " + vbCrLf + "Διεθνούς Ημέρα Γαλλοφωνίας."
                SunRise = "Ανατολή Ήλιου 6:28 Δύση Ήλιου 18:37"

            ElseIf DayOfYear = 81 Then
                ChurchFeast = "Μαρίας Αιγιπτίας, Ιακώβου ομολογητού, " + vbCrLf + "Φιλήμονος, Θωμά Κων/λεως, "
                NameFeast = "γιορτάζει ο Ιάκωβος. "
                WorldDay = "Παγκόσμια ημέρα κατα του ρατσισμού, " + vbCrLf + "Παγκόσμια ημέρα Δασοπονίας, " + vbCrLf + "Παγκόσμια ημέρα Ποίησης, " + vbCrLf + "Παγκόσμια ημέρα Ύπνου."
                SunRise = "Ανατολή Ήλιου 6:27 Δύση Ήλιου 18:38"

            ElseIf DayOfYear = 82 Then
                ChurchFeast = "Βασιλείου Αγκύρας, Καλλινίκης μάρτυρος, Δροσίδος, "
                NameFeast = "γιορτάζουν οι: Δρόσος, Δροσούλα. "
                WorldDay = "Παγκόσμια ημέρα για το Νερό."
                SunRise = "Ανατολή Ήλιου 6:25 Δύση Ήλιου 18:39"

            ElseIf DayOfYear = 83 Then
                ChurchFeast = "Νίκωνος και των συν αυτό 199 μαρτύρον. "
                NameFeast = ""
                WorldDay = " Παγκόσμια ημέρα Μετεωρολογίας."
                SunRise = "Ανατολή Ήλιου 6:24 Δύση Ήλιου 18:40"

            ElseIf DayOfYear = 84 Then
                ChurchFeast = "Αρτέμονος ιερομαρτ., Ζαχαρίου, Παρθενίου, "
                NameFeast = ""
                WorldDay = " Παγκόσμια ημέρα κατά της Φυματίωσης."
                SunRise = "Ανατολή Ήλιου 6:22 Δύση Ήλιου 18:41"

            ElseIf DayOfYear = 85 Then
                ChurchFeast = "+ΕΥΑΓΓΕΛΙΣΜΟΣ ΤΗΣ ΘΕΟΤΟΚΟΥ (Κατάλυσης ιχθύος)" + vbCrLf + "(Εθν.Εορτή). "
                NameFeast = "γιορτάζουν οι: Ευάγγελος, Ευαγγελία, Βαγγέλης," + vbCrLf + "Εθνεγερσία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:21 Δύση Ήλιου 18:42"

            ElseIf DayOfYear = 86 Then
                ChurchFeast = "+Σύναξις Αρχαγγέλου Γαβριήλ, Στεφάνου ομολ. " + vbCrLf + "(Κατάλυσης οίνου και ελαίου) "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:19 Δύση Ήλιου 18:43"

            ElseIf DayOfYear = 87 Then
                ChurchFeast = "Μαρτώνης, Φιλητού,Λυδίας, Παύλου Κορ. "
                NameFeast = "Γιορτάζουν οι: Αμφιλόχιος, Αμφιλοχία, Λυδία. "
                WorldDay = "Παγκόσμια ημέρα Θεάτρου."
                SunRise = "Ανατολή Ήλιου 6:18 Δύση Ήλιου 18:44"

            ElseIf DayOfYear = 88 Then
                ChurchFeast = "+Ιλαρίωνος οσίου, Ηρωδίωνος αποστ. εκ των 70, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:45"

            ElseIf DayOfYear = 89 Then
                ChurchFeast = "Μάρκου επ. Αρεθουσίων. Ευσταθίου, Κυρίλλου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:15 Δύση Ήλιου 18:46"

            ElseIf DayOfYear = 90 Then
                '30 Μαρτίου'
                ChurchFeast = "Ευβούλης, Ζαχαρίου επισκόπου Κορίνθου. "
                NameFeast = ""
                WorldDay = "Παγκόσμια ημέρα Αντισύλληψης"
                SunRise = "Ανατολή Ήλιου 6:13 Δύση Ήλιου 18:47"

            ElseIf DayOfYear = 91 Then
                ' 31 Μαρτίου'
                ChurchFeast = "Υπατίου επ. Γαγγρών, Ακακίου ιερομ. επ. Μελιτίνης. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:12 Δύση Ήλιου 18:47"

            ElseIf DayOfYear = 92 Then
                '1η Απριλίου'
                ChurchFeast = "Μαρίας Αιγυπτίας, Γεροντίου μ., Μακαρίου οσ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:10 Δύση Ήλιου 18:48"

            ElseIf DayOfYear = 93 Then
                ChurchFeast = "Τίνου οσ., Θεοδώρας μαρτ., Αιδεσίου, Αμφιανού μ. "
                NameFeast = ""
                WorldDay = " Παγκόσμια ημέρα Παιδικού Βιβλίου"
                SunRise = "Ανατολή Ήλιου 6:09 Δύση Ήλιου 18:49"

            ElseIf DayOfYear = 94 Then
                ChurchFeast = "Νικήτα ομολογητού, Ιωσήφ Υμνογράφου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:07 Δύση Ήλιου 18:50"

            ElseIf DayOfYear = 95 Then
                ChurchFeast = "Γεωργίου εν Μαλεώ, Ζωσιμά, Πλάτωνος "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:06 Δύση Ήλιου 18:51"

            ElseIf DayOfYear = 96 Then
                ChurchFeast = "Κλαυδίου, Διοδώρου μαρτ., Θεοδώρας Θεσ/νίκης. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα των Προσφύγων."
                SunRise = "Ανατολή Ήλιου 6:04 Δύση Ήλιου 18:52"

            ElseIf DayOfYear = 97 Then
                ChurchFeast = "Ευτυχίου Πατρ. Κων/λεως, Πλατωνίδος οσίας, "
                NameFeast = "γιορτάζουν οι: Ευτύχης, Ευτυχία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:03 Δύση Ήλιου 18:53"

            ElseIf DayOfYear = 98 Then
                ChurchFeast = "Γεωργίου οσ., Καλλιοπίου και Ακυλίνης μαρτ. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Υγείας."
                SunRise = "Ανατολή Ήλιου 6:01 Δύση Ήλιου 18:53"

            ElseIf DayOfYear = 99 Then
                ChurchFeast = "Αγάβου, Ερμού, Ρούφου μαρτύρων, Ηρωδίωνος. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:00 Δύση Ήλιου 18:55"

            ElseIf DayOfYear = 100 Then
                ChurchFeast = "Ευψυχίου μαρτ., Βαδίμου, Γριγοριου Ε Κων/λεως," + vbCrLf + "Τερεντίου, Δήμου νομ. "
                NameFeast = "Γιορτάζουν οι: Ευψύχιος, Ιωσήφ, Δημοσθένης, " + vbCrLf + "Ιωσηφίνα, Ραφαήλ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:58 Δύση Ήλιου 18:56"

            ElseIf DayOfYear = 101 Then
                '10 Απριλίου'
                ChurchFeast = "Ευψυχίου μαρτυρος, Βαδίμου οσιομάρτυρος, "
                NameFeast = "γιορτάζουν οι: Δημοσθένης, Διονύσιος, Περικλής, " + vbCrLf + "Ηρακλής."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:57 Δύση Ήλιου 18:57"

            ElseIf DayOfYear = 102 Then
                ChurchFeast = "Αντίπα Περγάμου-Τρυφαίνης οσίας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά της ασθενείας Πάρκινσον."
                SunRise = "Ανατολή Ήλιου 5:55 Δύση Ήλιου 18:58"

            ElseIf DayOfYear = 103 Then
                ChurchFeast = "Ανθούσης, Παρίου ομολογ., Ακακίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:54 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 104 Then
                ChurchFeast = "Μαρτίνου Πάπα Ρώμης, θεοχάρους. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:51 Δύση Ήλιου 19:00"

            ElseIf DayOfYear = 105 Then
                ChurchFeast = "Αριστάρχου, Πούδη, Τροφίμου, θομαΐδος, "
                NameFeast = "γιορτάζουν οι: Αρίσταρχος, Θομαΐς, Λεωνίδας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:50 Δύση Ήλιου 19:01"

            ElseIf DayOfYear = 106 Then
                ChurchFeast = "Λεωνίδου επισκ. Αθηνών, Κρήσκεντος μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Αρίσταρχος, Θωμαΐς, Λεωνήδας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:48 Δύση Ήλιου 19:02"

            ElseIf DayOfYear = 107 Then
                ChurchFeast = "Αγάπης, Ειρήνης, Χιονίας Μάρτυρος. "
                NameFeast = "Σήμερα γιορτάζει η Γαλήνη. "
                WorldDay = "Παγκόσμια Ημέρα Φωνής."
                SunRise = "Ανατολή Ήλιου 5:47 Δύση Ήλιου 19:03"

            ElseIf DayOfYear = 108 Then
                ChurchFeast = "Μακαρίου Κορίνθου, Συμεών επ. Περσίδος. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Αγροτικής Πάλης"
                SunRise = "Ανατολή Ήλιου 5:45 Δύση Ήλιου 19:04"

            ElseIf DayOfYear = 109 Then
                ChurchFeast = "Ιωάννου οσίου-Κυριλλου Κωνστ/λεως. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Πολοτιστικής Κληρονομιάς."
                SunRise = "Ανατολή Ήλιου 5:44 Δύση Ήλιου 19:05"

            ElseIf DayOfYear = 110 Then
                ChurchFeast = "Παφνουτίου ιερομ. Φιλίππας μάρτυρος. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:44 Δύση Ήλιου 19:05"

            ElseIf DayOfYear = 111 Then
                '20 Απριλίου'
                ChurchFeast = "Θεοδώρου τριχινά, Ζακχαίου, Αθανασίου. "
                NameFeast = ""
                WorldDay = " Διεθνής Ημέρα Ευαισθητοποίησης για το Θόρυβο."
                SunRise = "Ανατολή Ήλιου 5:43 Δύση Ήλιου 19:06"

            ElseIf DayOfYear = 112 Then
                ChurchFeast = "Ιανουαρίου ιερομ., Αναστασίου Σιναΐτου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:41 Δύση Ήλιου 19:07"

            ElseIf DayOfYear = 113 Then
                ChurchFeast = "Θεοδώρου οσ., Ναθαναήλ επισκ., Νεάρχου. "
                NameFeast = ""
                WorldDay = " Ημέρα της Γης."
                SunRise = "Ανατολή Ήλιου 5:40 Δύση Ήλιου 19:08"

            ElseIf DayOfYear = 114 Then
                ChurchFeast = "Λαζάρου Νεομάρτυρος"
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα βιβλίου."
                SunRise = "Ανατολή Ήλιου 5:39 Δύση Ήλιου 19:09"

            ElseIf DayOfYear = 115 Then
                ChurchFeast = "Ελισάβετ οσίας, Ξενοφώντος εν Άθω, "
                NameFeast = "γιορτάζουν οι: Αχιλλέας, Βάϊος, Βάϊα, Δάφνη, " + vbCrLf + "Ελισάβετ. "
                WorldDay = "Παγκόσμια Ημέρα κατάργησης Πειραμάτων σε Ζώα"
                SunRise = "Ανατολή Ήλιου 5:37 Δύση Ήλιου 19:10"

            ElseIf DayOfYear = 116 Then
                ChurchFeast = "+Μάρκου αποστόλου, Νίκης Μάρτυρος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Μάρκος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:36 Δύση Ήλιου 19:11"

            ElseIf DayOfYear = 117 Then
                ChurchFeast = "Βασιλείου Αμασείας, Γλαφύρας, Ιούστας, "
                NameFeast = "γιορτάζει η Γλαφύρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:35 Δύση Ήλιου 19:11"

            ElseIf DayOfYear = 118 Then
                ChurchFeast = "Συμεών Ιεροσολύμων ιερομάρτυρας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Σχεδίου (Desing)."
                SunRise = "Ανατολή Ήλιου 5:34 Δύση Ήλιου 19:12"

            ElseIf DayOfYear = 119 Then
                ChurchFeast = "Των εν Κυζίκω 9 Μαρτύρων-Θεόγνιδος και" + vbCrLf + "των συν αυτώ. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα για την Υγεία και την " + vbCrLf + "Ασφάλεια στην Εργασία."
                SunRise = "Ανατολή Ήλιου 5:32 Δύση Ήλιου 19:13"

            ElseIf DayOfYear = 120 Then
                ChurchFeast = "Ιάσονος και Σωσιπάτρου, Ιωαννου Καλοκτένους, "
                NameFeast = "γιορτάζουν οι: Ιάσων, Κέρκυρα. "
                WorldDay = "Παγκόσμια Ημέρα Χορού."
                SunRise = "Ανατολή Ήλιου 5:31 Δύση Ήλιου 19:14"

            ElseIf DayOfYear = 121 Then
                ChurchFeast = "+Ιακώβου αποστόλου-Κλήμεντος οσίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ασημάκης, Ασημίνα, Δονάτος, " + vbCrLf + "Ιάκωβος. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:30 Δύση Ήλιου 19:15"

            ElseIf DayOfYear = 122 Then
                '1η Μαΐου'
                ChurchFeast = "Ιερεμίου προφ., Ευθυμίου, "
                NameFeast = "γιορτάζει ο Ιερεμίας. "
                WorldDay = "ΕΡΓΑΤΙΚΗ ΕΟΡΤΗ ΠΡΩΤΟΜΑΓΙΑ."
                SunRise = "Ανατολή Ήλιου 5:29 Δύση Ήλιου 19:16"

            ElseIf DayOfYear = 123 Then
                ChurchFeast = "+Ανακομιδή λειψάνον Μεγ. Αθανασίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Έσπερος, Εσπέρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:26 Δύση Ήλιου 19:17"

            ElseIf DayOfYear = 124 Then
                ChurchFeast = "Τιμοθέου και Μαύρας, Πέτρου Άργους, "
                NameFeast = "γιορτάζει η Ροδόπη "
                WorldDay = "Παγκόσμια Ημέρα Ελευθεροτυπίας Παγκόσμια " + vbCrLf + "ημέρα κατά του Άσθματος."
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:18"

            ElseIf DayOfYear = 125 Then
                ChurchFeast = "Πελαγίας, Ιλαρίου οσίου, Αντωνίου μαρτ. "
                NameFeast = "Γιορτάζουν οι: Πελαγία, Θεοχάρης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:25 Δύση Ήλιου 19:19"

            ElseIf DayOfYear = 126 Then
                ChurchFeast = "+Ειρήνης Μεγαλομ., Ευθυμίου Μαδύτου, "
                NameFeast = "γιορτάζουν οι: Ειρήνη, Ρένα, Ειρηναίος, " + vbCrLf + "Ειρηναία, Ευραίμ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:24 Δύση Ήλιου 19:20"

            ElseIf DayOfYear = 127 Then
                ChurchFeast = "Ιώβ του πολυάθλου, Σεραφιμ οσίου, "
                NameFeast = "γιορτάζουν οι: Ζήσης, Ζωή, Ζώης, Πηγή Σεραφείμ. "
                WorldDay = "Παγκόσμια Ημέρα κατά της Δίαιτας."
                SunRise = "Ανατολή Ήλιου 5:23 Δύση Ήλιου 19:21"

            ElseIf DayOfYear = 128 Then
                ChurchFeast = "Του εν Ουρανώ Φανέντος Τιμίου Σταυρού. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Γέλιου."
                SunRise = "Ανατολή Ήλιου 5:22 Δύση Ήλιου 19:22"

            ElseIf DayOfYear = 129 Then
                ChurchFeast = "+Ιωάννου του Θεολόγου και Ευαγγελιστού-Αρσενίου" + vbCrLf + "Μεγ. (Κατάλυσης οίνου και ελαίου) "
                NameFeast = "Γιορτάζει ο Θεολόγος. "
                WorldDay = "Παγκόσμια Ημέρα Ερυθρού Σταυρού και Ερυθράς " + vbCrLf + "Ημισελήνου. "
                SunRise = "Ανατολή Ήλιου 5:21 Δύση Ήλιου 19:23"

            ElseIf DayOfYear = 130 Then
                ChurchFeast = "+ Χριστοφόρου Μάρτυρος-Ησαΐου προφήτ. "
                NameFeast = "Γιορτάζουν οι: Ησαΐας, Χριστόφορος. "
                WorldDay = "Ημέρα της Ευρώπης."
                SunRise = "Ανατολή Ήλιου 5:20 Δύση Ήλιου 19:23"

            ElseIf DayOfYear = 131 Then
                '10 Μαΐου'
                ChurchFeast = "Σίμωνος Ζηλωτού, Λαυρεντίου, Αλφειού, "
                NameFeast = "γιορτάζει ο Σίμος. "
                WorldDay = "Παγκόσμια Ημέρα της Μητέρας."
                SunRise = "Ανατολή Ήλιου 5:19 Δύση Ήλιου 19:24"

            ElseIf DayOfYear = 132 Then
                ChurchFeast = "Κυρίλου και Μεθοδίου, Μωκίου, Διοσκόρου, "
                NameFeast = "γιορτάζουν οι: Μεθόδιος, Ολυμπία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:18 Δύση Ήλιου 19:25"

            ElseIf DayOfYear = 133 Then
                ChurchFeast = "Επιφανείου Κύπρ., Γερμανού, Θεοδώρου. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Αδελφών Νοσοκόμων."
                SunRise = "Ανατολή Ήλιου 5:17 Δύση Ήλιου 19:26"

            ElseIf DayOfYear = 134 Then
                ChurchFeast = "Ευθυμίου Ιβήρων, Γλυκερίας, Σεργίου ομολογητού, "
                NameFeast = "γιορτάζει η Γλυκερία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:27"

            ElseIf DayOfYear = 135 Then
                ChurchFeast = "Θεράποντος, Ισιδώρου εν Χίω, Λεοντίου, Ιωάννου, "
                NameFeast = "γιορτάζουν οι: Ισίδωρος, Ισιδώρα, Αριστοτέλης, Τέλης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:28"

            ElseIf DayOfYear = 136 Then
                ChurchFeast = "+ Παχωμίου Μεγαλομ., Αχιλλίου Λαρίσης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Αχίλιος, Κάλη, Καλη. "
                WorldDay = "Διεθνής Ημέρα Οικογένειας."
                SunRise = "Ανατολή Ήλιου 5:15 Δύση Ήλιου 19:29"

            ElseIf DayOfYear = 137 Then
                ChurchFeast = "Θεοδώρου Ηγιασμένου, Γεωργίου Μυτιλ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:14 Δύση Ήλιου 19:30"

            ElseIf DayOfYear = 138 Then
                ChurchFeast = "Ανδρονίκου και Ιουνίας απ. εκ των 70, " + vbCrLf + "Αθανασίου Χριστιαν, "
                NameFeast = "γιορτάζουν οι: Ανδρόνικος, Ανδρονίκη, Ιουνία. "
                WorldDay = "Παγκόσμια Ημέρα Τηλεπικοινωνιων."
                SunRise = "Ανατολή Ήλιου 5:13 Δύση Ήλιου 19:30"

            ElseIf DayOfYear = 139 Then
                ChurchFeast = "Πέτρου-Διονυσίου, Ανδρέου, Ιουλίας μάρτ. "
                NameFeast = "Γιορτάζουν οι: Γαλάτεια, Ιουλία. "
                WorldDay = "Παγκόσμια Ημέρα Μουσείων."
                SunRise = "Ανατολή Ήλιου 5:12 Δύση Ήλιου 19:31"

            ElseIf DayOfYear = 140 Then
                ChurchFeast = "Πατρικίου Προύσσης, Μενάνδρου μάρτ. "
                NameFeast = "Γιορτάζουν οι: Μαγδαληνή, Μάγδα, Πατρίκιος. "
                WorldDay = "Ημέρα μνήμης της γενοκτονίας του Ποντιακού " + vbCrLf + "Ελληνισμού."
                SunRise = "Ανατολή Ήλιου 5:12 Δύση Ήλιου 19:32"

            ElseIf DayOfYear = 141 Then
                '20 Μαΐου'
                ChurchFeast = "+ Αν. Λειψ. Αγ. Νικολάου, Νικήτα Χίου, " + vbCrLf + "Θαλλελαίου μαρτ. "
                NameFeast = "Γιορτάζει η Λυδία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:11 Δύση Ήλιου 19:33"

            ElseIf DayOfYear = 142 Then
                ChurchFeast = "+Κωνσταντίνου και Ελένής, Βισαρίωνος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι:Ελένη, Ελεάννα, Κωωσταντίνος," + vbCrLf + "Κωνσταντίνα. "
                WorldDay = "Παγκόσμια Ημέρα Πολιτισμου."
                SunRise = "Ανατολή Ήλιου 5:10 Δύση Ήλιου 19:34"

            ElseIf DayOfYear = 143 Then
                ChurchFeast = "Πάυλου και Δημητρίου Τριπόλ., Βασιλίσκου, "
                NameFeast = "γιορτάζουν οι: Αιμίλιος, Αιμιλία, Εμυ, Εμιλία, " + vbCrLf + "Επιφάνειος. "
                WorldDay = "Παγκόσμια Ημέρα Βιοποικιλότητας."
                SunRise = "Ανατολή Ήλιου 5:09 Δύση Ήλιου 19:35"

            ElseIf DayOfYear = 144 Then
                ChurchFeast = "Μιχαήλ Συνάδων, Συνεσίου Επισκόπου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:09 Δύση Ήλιου 19:35"

            ElseIf DayOfYear = 145 Then
                ChurchFeast = "Συμεών οσίου, Μαρκιανής, Σωσάννης, "
                NameFeast = "γιορτάζει ο Μέλέτης. "
                WorldDay = "Ευρωπαϊκή Ημέρα Πάρκων."
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 146 Then
                ChurchFeast = "+Γ' Εύρεσις Κεφαλής Τιμίου Προδρόμου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα αλληλεγγύης στους λαούς," + vbCrLf + "που αγωνίζονται για την Ελευθερία Ανεξαρτισία " + vbCrLf + "και τα Ανθρώπινα Δικαιώματα, " + vbCrLf + "Παγκόσμια Ημέρα Αφρικής."
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 147 Then
                ChurchFeast = "Κάρπου και Αλφαίου εκ των 70 Απ., Αλεξάνδρου νεομ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:37"

            ElseIf DayOfYear = 148 Then
                ChurchFeast = "+ Ιωάννου Ρώσου, Ελλαδίου ιερ.,Θεράποντος ιερομ. "
                NameFeast = "Γιορτάζουν οι: Αλύπιος, Αλυπία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:07 Δύση Ήλιου 19:38"

            ElseIf DayOfYear = 149 Then
                ChurchFeast = "Ευτυχούς επ. Μελιτινής, Ελικωνίδος, " + vbCrLf + "Νικήτα Χαλκηδόνας. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:39"

            ElseIf DayOfYear = 150 Then
                ChurchFeast = "Θεοδοσίας Παρθενομάρτυρος, Υπομονής οσίας, "
                NameFeast = "Σήμερα γιορτάζει η Θεοδοσία. "
                WorldDay = "Παγκόσμια Ημέρα Κυανοκράνων."
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:40"

            ElseIf DayOfYear = 151 Then
                '30 Μαΐου'
                ChurchFeast = "Ισακίου ηγουμ. Μονής Δαλμάτων, Εμμελείας μητρός" + vbCrLf + "Μ. Βασιλείου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:41"

            ElseIf DayOfYear = 152 Then
                '31 Μάΐου'
                ChurchFeast = "Ερμείου Αποστόλου και μάρτυρος, ευσεβίου μ., " + vbCrLf + "Ευσταθίου Κων/λεως. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Κατά του Καπνίσματος."
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:41"

            ElseIf DayOfYear = 153 Then
                '1η Ιουνίου'
                ChurchFeast = "Ιουστίνου Μάρτυρος, Πύρρου οσίου, "
                NameFeast = "γιορτάζουν οι: Γεράκης, Γερακία, Ευέλπιστος, " + vbCrLf + "Ιουστίνος, Πύρρος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:42"

            ElseIf DayOfYear = 154 Then
                ChurchFeast = "Νικηφόρου Κωνστ., Εράσμου, Κωνσταντήνου. "
                NameFeast = "γιορτάζουν οι: Νικηφόρος, Μαρίνος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:43"

            ElseIf DayOfYear = 155 Then
                ChurchFeast = "Λουκιλλιανόυ και Πάύλης και των συν αυτοίς. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:43"

            ElseIf DayOfYear = 156 Then
                ChurchFeast = "Μάρθας και Μαρίας αδ. Λαζάρου, Μητροφάνους " + vbCrLf + "Κωνσ/λεως, "
                NameFeast = "γιορτάζει η Μάρθα. "
                WorldDay = "Διεθνής Ημέρα κατά της επιθετικότητας εναντίον " + vbCrLf + "των Παιδιών."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:44"

            ElseIf DayOfYear = 157 Then
                ChurchFeast = "Δωροθέου επ. Τύρου, Νικανδρου, Γοργίου. "
                NameFeast = "γιορτάζουν οι: Απόλλων, Δωρόθέα. "
                WorldDay = "Παγκόσμια Ημέρα Περιβάλοντος."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 158 Then
                ChurchFeast = "Θεοδώρου Ηγιασμένου, Ιλαρίονος Δαλμάτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 159 Then
                ChurchFeast = "Θεοδότου Αγκ., Ζηναϊδος, Σεβαστιανής. "
                NameFeast = "γιορτάζει η Σεβαστιανή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:46"

            ElseIf DayOfYear = 160 Then
                ChurchFeast = "+Ανακομιδή λειψάνον Θεοδώρου Στρατηλάτου, " + vbCrLf + "Καλλιόπης (Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει η Καλλιόπη. "
                WorldDay = "Παγκόσμια Ημέρα των Ωκεανών."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:46"

            ElseIf DayOfYear = 161 Then
                ChurchFeast = "Κυρίλου αρχιεπισκόπου Αλεξανδρείας, " + vbCrLf + "Ανανίου μαρτ., Θεοφάνους Εγκ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 162 Then
                '10 Ιουνίου'
                ChurchFeast = "Αλεξάνδρου-Αντονίνης μάρτυρος Τιμοθέου Προύσης, "
                NameFeast = "γιορτάζει η Σάρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 163 Then
                ChurchFeast = "+Βαρθολομαίου και Βαρνάβα των Αποστόλων," + vbCrLf + "Λουκά Ρώσου Ιατρού (Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βαρθολομαίος, Βαρνάβας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 164 Then
                ChurchFeast = "Ονουφρίου οσίου Πέτρου εν Άθω, "
                NameFeast = "γιορτάζει ο Ονούφριος. "
                WorldDay = "Παγκόσμια Ημέρα κατά της Παιδικής Εργασίας."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 165 Then
                ChurchFeast = "Ακυλίνης, Αντιπάτρου, τριφυλλίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 166 Then
                ChurchFeast = "Ελισσαίου προφήτου, Μεθοδίου, κυρίλλου, "
                NameFeast = "γιορτάζει ο Ελισσαίος. "
                WorldDay = "Παγκόσμια Ημέρα του εθελοντού Αιμοδότη, Διεθνής" + vbCrLf + "Ημέρα των Webloggers."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 167 Then
                ChurchFeast = "Αμώς προφ., Ιερωνυμου, Στεφανά, Ιωνά, "
                NameFeast = "γιορτάζουν οι: Αυγουστίνα, Αυγουστίνος, " + vbCrLf + "Αύγουστος, Μόνικα. "
                WorldDay = "Παγκόσμια Ημέρα του Πατέρα Παγκόσμια Ημέρα " + vbCrLf + "Γονιμότητας."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 168 Then
                ChurchFeast = "Τύχωνος, Μάρκου, Απολλων Ιάδος. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 169 Then
                ChurchFeast = "Ισαύρου, Μανουήλ, Σαβέλ, Ισμαήλ μαρτύρων. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά της Ερημοποίησης και της " + vbCrLf + "Ξηρασίας."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 170 Then
                ChurchFeast = "Λεοντίου, Υπατίου Θεοδούλου Αιθερίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 171 Then
                ChurchFeast = "Ιούδα Θαδδαίου Θεαδέλφου, Ζήνωνος, "
                NameFeast = "γιορτάζει ο Παΐσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 172 Then
                '20 Ιουνίου'
                ChurchFeast = "Μεθοδίου Πατάρων, Νικολάου Καβάσιλα, "
                NameFeast = "γιορτάζουν οι: Κορίνα, Κόρη. "
                WorldDay = "Παγκόσμια Ημέρα Προσφύγων."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 173 Then
                ChurchFeast = "Ιουλιανού, Νικήτα, Τερεντίου, Αφροδισίου. "
                NameFeast = "γιορτάζουν οι: Αφροδίσιος, Αφροδισία. "
                WorldDay = "Ευρωπαϊκή Ημέρα Μουσικής, Παγκόσμια Ημέρα " + vbCrLf + "Υδρογραφίας."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 174 Then
                ChurchFeast = "Ευσεβίου, Ζήνονος, Ζηνά, Ιουλιανής, "
                NameFeast = "γιορτάζουν οι: Ευσέβιος, Σέβη, Ευσεβούλα, Ζήνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 175 Then
                ChurchFeast = "Αγριππίνης μάρτυρος, Αριστοκλέους, "
                NameFeast = "γιορτάζουν οι: Αγριππίνα, Αριστοκλής, Λουλού," + vbCrLf + "Λούλης. "
                WorldDay = "Ολυμπιακή Ημέρα"
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 176 Then
                ChurchFeast = "+ Γέννεση Ι.Προδρόμου, Παναγιώτου νεομάρτυρος " + vbCrLf + "(Κατάλυσης ιχθύος). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 177 Then
                ChurchFeast = "Φεβρωνίας, ευφροσύνης, Μεθοδίου, "
                NameFeast = "γιορτάζουν οι: Έρως, Έρωτας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 178 Then
                ChurchFeast = "Δαβιδ Θεσαλονίκης, Ιωάνου Γοτθίας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά των ναρκωτικών, " + vbCrLf + "Διεθνής Ημέρα κατά των Βασανισμών."
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 179 Then
                ChurchFeast = "Σαμψών Ξενοδόχου, Ιοάννας Μυροφόρου. "
                NameFeast = ""
                WorldDay = " Ημέρα Ομοφυλόφιλης Υπερηφάνειας."
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 180 Then
                ChurchFeast = "Ανακ. Λειψάνον Κύρου και Ιωάννου Αναργύρων, "
                NameFeast = "γιορτάζει ο Γερμανός"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 181 Then
                ChurchFeast = "+ ΠΕΤΡΟΥ ΚΑΙ ΠΑΥΛΟΥ Των αποστόλων " + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν  οι: Πέτρος, Παύλος, Παυλίνα, " + vbCrLf + "Πετρούλα. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 182 Then
                '30 Ιουνίου'
                ChurchFeast = "+ Η Σύναξις των Δώδεκα Αποστόλων " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Απόστολος, Αποστολία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 183 Then
                '1η Ιουλίου'
                ChurchFeast = "+Κοσμα και Δαμιανού των αναργύρων " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ανάργυρος, Ανάργυρη, Δαμιανός, " + vbCrLf + "Κοσμάς."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 184 Then
                ChurchFeast = "+Καταθ. Τιμίας Εσθήτος Θεοτ, Ιουβεναλίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:07 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 185 Then
                ChurchFeast = "Υακίνθου, Ανατολίου, Γερασίμου Καρπεν, "
                NameFeast = "γιορτάζουν οι: Ζουμπουλία, Υάκινθος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:07 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 186 Then
                ChurchFeast = "Ανδρέου Κρήτης, Λουκίας, Θεοδότου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 187 Then
                ChurchFeast = "Αθανασίου εν Άθω, Λαμπαδού, Σεργίου. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Συνεταιρισμών."
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 188 Then
                ChurchFeast = "Σισώη, Αρχίππου, Επιμάχου, Στεφάνου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:09 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 189 Then
                ChurchFeast = "+ Κυριακής Μεγαλομ., Λουκιανού, Θωμά, "
                NameFeast = "γιορτάζουν οι: Κυριακή, Κική, Κίκα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:10 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 190 Then
                ChurchFeast = "+ Προκοπίου Μεγαλομ., Θεοδοσίας μάρτ. "
                NameFeast = "Γιορτάζουν οι: Θεόφιλος, Προκόπης. "
                WorldDay = "Παγκόσμια Ημέρα Αλλεργίας."
                SunRise = "Ανατολή Ήλιου 5:10 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 191 Then
                ChurchFeast = "Παγκρατίου ιερομ, Πρόβου, Μεθοδίου, "
                NameFeast = "γιορτάζει η Βερόνικα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:11 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 192 Then
                '10 Ιουλίου'
                ChurchFeast = "Εν Νικοπόλει 45 Μαρτ. Απολλωνίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:11 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 193 Then
                ChurchFeast = "+ Ευφημίας Μεγαλομ., Όλγας, Νεκταρίου, "
                NameFeast = " γιορτάζει οι: Ευφημία, Όλγα. "
                WorldDay = "Παγκόσμια Ημέρα Πληθυσμού"
                SunRise = "Ανατολή Ήλιου 5:12 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 194 Then
                ChurchFeast = "Πρόκλου, Ιλαρίου, Βερονίκης, Σάββα. "
                NameFeast = "γιορτάζουν οι: Βερενίκη, Βερόνικα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:13 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 195 Then
                ChurchFeast = "Σύν.Αρχ. Γαβριήλ, Στεφάνου Σαβαΐτου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:13 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 196 Then
                ChurchFeast = "+ Νικοδήμου οσ., Ιωσήφ Θεσ/νίκης, Ακύλα. "
                NameFeast = "Σήμερα γιοτράζουν οι: Ακύλας, Νικόδημος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:14 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 197 Then
                ChurchFeast = "Κηρύκου και Ιουλίττης, Ματρώνης Χίου, "
                NameFeast = "γιορτάζουν οι: Βλαδίμηρος, Ιουλίττα, Κήρυκος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:15 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 198 Then
                ChurchFeast = "Αθηνογένους, Αντιόχου μαρτ., Φαύστου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 199 Then
                ChurchFeast = "+ Μαρίνης μεγαλομ., Βερονίκης, Σπεραίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Μάρίνα, Μαρίνος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:46"

            ElseIf DayOfYear = 200 Then
                ChurchFeast = "Αιμιλιανού, Υακίνθου, Ουαλεντίνης μαρτ. "
                NameFeast = "Γιορτάζουν οι: Αιμιλιανός, Αιμίλιος, " + vbCrLf + "Αιμιλιανή, Αιμιλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:17 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 201 Then
                ChurchFeast = "Μακρίνης και Δίου, Ευγενίου, Στεφάνου, "
                NameFeast = "γιορτάζουν οι: Διός, Δίας"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:18 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 202 Then
                ' 20 Ιουλίου'
                ChurchFeast = "+ Προφήτου Ηλίου Θεσβίτου, Φλαβιανού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ηλίας"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:19 Δύση Ήλιου 19:44"

            ElseIf DayOfYear = 203 Then
                ChurchFeast = "Ιωάννου, Συμεόν του Σαλού, Ονουφρίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:19 Δύση Ήλιου 19:43"

            ElseIf DayOfYear = 204 Then
                ChurchFeast = "+Μαρίας Μαγδαλην. Μυροφ., Μαρκέλλης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορταζουν οι: Μαρκέλα, Μαγδαληνή, Μάγδα. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:21 Δύση Ήλιου 19:42"

            ElseIf DayOfYear = 205 Then
                ChurchFeast = "Φωκά ιερομ., Ιεζεκιήλ, Βιταλίου, Σωσάννης. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:22 Δύση Ήλιου 19:41"

            ElseIf DayOfYear = 206 Then
                ChurchFeast = "Χριστίνης, Αφηναγόρου, Υμεναίου μάρτυρως, "
                NameFeast = "γιορτάζει ο Αθηναγόρας"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:22 Δύση Ήλιου 19:40"

            ElseIf DayOfYear = 207 Then
                ChurchFeast = "+Κοίμησις Αγίας Άννης, Ευπραξίας " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Άννα, Ανούλα, Αννίτα, " + vbCrLf + "Αννέτα, Ευπραξία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:23 Δύση Ήλιου 19:40"

            ElseIf DayOfYear = 208 Then
                ChurchFeast = "+ Παρασκευής οσιομάρτυρος, Ερμολάου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει η Παρασκευή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:24 Δύση Ήλιου 19:39"

            ElseIf DayOfYear = 209 Then
                ChurchFeast = "+Παντελεήμονος Ιαματικού, Ανθούσης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = " γιορτάζει ο Παντελής."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:25 Δύση Ήλιου 19:38"

            ElseIf DayOfYear = 210 Then
                ChurchFeast = "Προχόρου, Νικάνορος, Ερήνης Χρυσοβ, "
                NameFeast = "γιορτάζουν οι Ακάκιος, Αυξέντιος, Αυξεντία," + vbCrLf + "Δρόσος, Δροσούλα. "
                WorldDay = "Διεθνής Ημέρα του Διαχειριστή Συστημάτων"
                SunRise = "Ανατολή Ήλιου 5:26 Δύση Ήλιου 19:37"

            ElseIf DayOfYear = 211 Then
                ChurchFeast = "+ Καλλινίκου και Θεοδότης, Βασιλίσκου, "
                NameFeast = "γιορτάζει ο Καλλίνικος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 212 Then
                '30 Ιουλίου'
                ChurchFeast = "Σίλα, Ανδρονίκου Επαινετού, Σιλουανού, "
                NameFeast = "γιορτάζουν οι: Ανδρόνικος, Ανδρονίκη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 213 Then
                ChurchFeast = "Προεόρτια Προόδου Τιμίου Σταυρού,"
                NameFeast = " γιορτάζουν οι: Ευδόκιμος, Ιωσήφ, Σήφις, " + vbCrLf + "Ιωσηφίνα, Ζοζεφίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:35"

            ElseIf DayOfYear = 214 Then
                '1η Αυγούστου'
                ChurchFeast = "+Πρόοδος Τιμίου Σταυρου (αρχή νηστείας)., " + vbCrLf + "7 Παίδων, Σολομονής. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:28 Δύση Ήλιου 19:34"

            ElseIf DayOfYear = 215 Then
                ChurchFeast = "Ανακομ. λειψάνου Στεφάνου, Θεοδωρου νεομ."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:29 Δύση Ήλιου 19:33"

            ElseIf DayOfYear = 216 Then
                ChurchFeast = "Δαλμάτου, Φαύστρου, Ισαακίου, Σαλώμης, "
                NameFeast = "γιορτάζει η Σαλώμη. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:30 Δύση Ήλιου 19:32"

            ElseIf DayOfYear = 217 Then
                ChurchFeast = "Των εν Εφέσω 7 Παίδων, Ευδοκίας. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:31 Δύση Ήλιου 19:31"

            ElseIf DayOfYear = 218 Then
                ChurchFeast = "Ευσιγνίου, Ευθυμίου, Νόνας, Φαβίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:32 Δύση Ήλιου 19:31"

            ElseIf DayOfYear = 219 Then
                ChurchFeast = "+ Η ΜΕΤΑΜΟΡΦΩΣΙΣ ΤΟΥ ΣΩΤΗΡΟΣ (Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν  οι: Σωτήριος, Σωτηρία, Ευμορφία, " + vbCrLf + "Πορφούλα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:32 Δύση Ήλιου 19:30"

            ElseIf DayOfYear = 220 Then
                ChurchFeast = "Δομετίου, Θεοδοσίου, Αργολίδος, "
                NameFeast = "γιορτάζουν οι: Αστέριος, Αστέρης, Αστέρω, " + vbCrLf + "Αστερία, Αστρινή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:33 Δύση Ήλιου 19:29"

            ElseIf DayOfYear = 221 Then
                ChurchFeast = "Αιμιλιανού, Μύρωνος, τριανταφύλλου, "
                NameFeast = "γιορτάζουν οι Τριαντάφυλλος, Τριανταφυλλιά."
                WorldDay = "Παγκόσμια Ημέρα Οργασμού"
                SunRise = "Ανατολή Ήλιου 5:34 Δύση Ήλιου 19:28"

            ElseIf DayOfYear = 222 Then
                ChurchFeast = "Ματθία αποστ., Ψόη, Μαρίας Πατρκίας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα των Αυτοχθόνων Λαών του Κόσμου."
                SunRise = "Ανατολή Ήλιου 5:35 Δύση Ήλιου 19:25"

            ElseIf DayOfYear = 223 Then
                '10 Αυγούστου'
                ChurchFeast = "Λαυρεντίου αρχιδιακ., Ξύστου Ιππολύτου, "
                NameFeast = "γορτάζουν οι: Ευλαμπία, Ηρώ, Ιππόλυτος, Λαυρέντης," + vbCrLf + "Λαυρεντία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:36 Δύση Ήλιου 19:24"

            ElseIf DayOfYear = 224 Then
                ChurchFeast = "Εύπλου, Θάυμα Αγ. Σπυρίδων., Νεοφύτου"
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:37 Δύση Ήλιου 19:23"

            ElseIf DayOfYear = 225 Then
                ChurchFeast = "Ανικήτου, Παμφίλου, Φωτίου, Καπίτωνος. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Νεολαίας"
                SunRise = "Ανατολή Ήλιου 5:38 Δύση Ήλιου 19:22"

            ElseIf DayOfYear = 226 Then
                ChurchFeast = "Μαξίμου ομ., Ευδοκίας, Τύχωνος, Ξένης"
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Αριστερόχειρων"
                SunRise = "Ανατολή Ήλιου 5:39 Δύση Ήλιου 19:21"

            ElseIf DayOfYear = 227 Then
                ChurchFeast = "Προεόρτια Κοιμ.Θεοτ., Μιχαίου, Συμεών"
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:40 Δύση Ήλιου 19:19"

            ElseIf DayOfYear = 228 Then
                ChurchFeast = "+ Η ΚΟΙΜΗΣΙ ΤΗΣ ΘΕΟΤΟΚΟΥ (Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν οι: Μαρία, Μαριάνθη, Παναγιώτα, Παναγιώτης," + vbCrLf + "Μάριος, Δέσποινα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:40 Δύση Ήλιου 19:18"

            ElseIf DayOfYear = 229 Then
                ChurchFeast = "Διομήδους, Αλκιβιάδου, Αγ. Μανδηλιου, "
                NameFeast = "γιορτάζουν οι: Αλκιβιάδης, Αλκίνοος, Γεράσιμος, Σταμάτης"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:41 Δύση Ήλιου 19:17"

            ElseIf DayOfYear = 230 Then
                ChurchFeast = "Μύρωνος, Κυπριανού, Στράτωνος μάρτ., "
                NameFeast = "γιορτάζει η Λευκοθέα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:42 Δύση Ήλιου 19:15"

            ElseIf DayOfYear = 231 Then
                ChurchFeast = "Φλώρου, Λαύρου, Ερμού Λέοντος μάρτ., "
                NameFeast = "γιορτάζουν οι: Αρσένιος, Αρσένης, Αρσενία, Φλώρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:43 Δύση Ήλιου 19:14"

            ElseIf DayOfYear = 232 Then
                ChurchFeast = "Ανδρέου Στρατηλ., Θέκλης, Θεοφάνους. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:44 Δύση Ήλιου 19:13"

            ElseIf DayOfYear = 233 Then
                '20 Αυγούστου'
                ChurchFeast = "Σαμουήλ προφ., Ηλιοδωρου, Μέμνονος, "
                NameFeast = "γιορτάζει ο Ηλιόδωρος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:45 Δύση Ήλιου 19:11"

            ElseIf DayOfYear = 234 Then
                ChurchFeast = "Θαδδαίου αποστ., Βάσσης, Θεοκλητούς. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:46 Δύση Ήλιου 19:10"

            ElseIf DayOfYear = 235 Then
                ChurchFeast = "Αγαθονίκου, Ακινδύνου, Ανθούσης μάρτ., "
                NameFeast = "γιορτάζει ο Αγαθώνικος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:47 Δύση Ήλιου 19:09"

            ElseIf DayOfYear = 236 Then
                ChurchFeast = "+ Αποδ.Κοιμ. Θεοτ., Παναγίας Προύσσης"
                NameFeast = ""
                WorldDay = "Διεθνής Ημέρα κατά του δουλεμπορίου"
                SunRise = "Ανατολή Ήλιου 5:47 Δύση Ήλιου 19:07"

            ElseIf DayOfYear = 237 Then
                ChurchFeast = "+Κοσμά Αιτωλού, Ανακ. Λειψάν. Διονυσίου, "
                NameFeast = "γιορτάζουν οι: Αιτωλία, Ευτύχης, Ευτυχία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:48 Δύση Ήλιου 19:06"

            ElseIf DayOfYear = 238 Then
                ChurchFeast = "Βαρθολομαίου και Τίτου Αποστ., ευμενίου, "
                NameFeast = "γιορτάζει ο Βαρθολομαίος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:49 Δύση Ήλιου 19:04"

            ElseIf DayOfYear = 239 Then
                ChurchFeast = "Αδριανου και Ναταλίας, Ιωασάφ, Αττικού, "
                NameFeast = "γιορτάζουν οι: Αντριάνα, Ανδριανός, Ναταλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:50 Δύση Ήλιου 19:00"

            ElseIf DayOfYear = 240 Then
                ChurchFeast = "+ Φανουρίου Μεγαλομ., Ποιμένος, Ανθούσης, "
                NameFeast = "γιορτάζουν οι: Φανούρης, Φάνης, Φανή, Αρκαδία," + vbCrLf + "Αρκάδα, Αρκάδιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:51 Δύση Ήλιου 19:00"

            ElseIf DayOfYear = 241 Then
                ChurchFeast = "Μωϋσέως Αιθίοπ., Διομήδους, Λαυρεντίου, "
                NameFeast = "γιορτάζουν οι: Δάμων, Ευκίνη. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:52 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 242 Then
                ChurchFeast = "+Αποτομή Κεφ. Ιοάννου και βαπτιστού, Θεοπίστης οσ.(νηστεία). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:53 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 243 Then
                ' 30 Αυγούστου'
                ChurchFeast = "Αλεξάνδρου, Ιωάνου και Πάύλου Πατριαρχ.Κων/λεως, "
                NameFeast = "γιορτάζουν οι: Αλέξανδρος, Αλέκος, Ευλάλιος, Ζηνοβία."
                WorldDay = "Διεθνής Ημέρα Εξαφανισμένων"
                SunRise = "Ανατολή Ήλιου 5:53 Δύση Ήλιου 18:57"

            ElseIf DayOfYear = 244 Then
                ChurchFeast = "+Κατάθεσις Τιμίας Ζώνης Υπεραγίας, Θεοτόκου, " + vbCrLf + "Αριστείδου (Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:54 Δύση Ήλιου 18:56"

            ElseIf DayOfYear = 245 Then
                '1η Σεμτεμβρίου'
                ChurchFeast = "+Αρχή Ινδίκτου, Μελετίου οσ., Συμεών Στυλίτου," + vbCrLf + "Μελέτιου (Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Συμεών, Αθηνά, Αντιγώνη, Ερατώ, " + vbCrLf + "Θάλεια, Κλειώ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:55 Δύση Ήλιου 18:54"

            ElseIf DayOfYear = 246 Then
                ChurchFeast = "Μαμάντος, Φιλαδέλφου, Ιωάνν. νηστευτού, "
                NameFeast = "Μαμάντος, Φιλαδέλφου, Ιωάνν. νηστευτού."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:56 Δύση Ήλιου 18:53"

            ElseIf DayOfYear = 247 Then
                ChurchFeast = "Ανθίμου ιερομ., Θεοκτίστου, Πολυδώρου, "
                NameFeast = "γιορτάζουν οι: Ανθιμος, Αρίστη, Αριστίων, " + vbCrLf + "Αρχοντή, Αρχοντία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:57 Δύση Ήλιου 18:51"

            ElseIf DayOfYear = 248 Then
                ChurchFeast = "Βαβύλα, Μωϋσέως πρ., Ερμιόνης, Βεβαίας, "
                NameFeast = "γιορτάζουν οι: Ερμιόνη, Μοϋσής, Ροζαλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:58 Δύση Ήλιου 18:50"

            ElseIf DayOfYear = 249 Then
                ChurchFeast = "Ζαχαρίου, Αβδαίου, Ουβρανού, Θεοδώρου, "
                NameFeast = "γιορτάζει ο Ζαχαρίας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:59 Δύση Ήλιου 18:48"

            ElseIf DayOfYear = 250 Then
                ChurchFeast = "+Ανάμνησις θαύματος Αρχιστρατήγου Μιχαήλ " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βίβος, Βιβή, Ευδόξιος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:59 Δύση Ήλιου 18:47"

            ElseIf DayOfYear = 251 Then
                ChurchFeast = "Ευόδου και Ονησιφόρου, Σώζοντος μάρτυρος, "
                NameFeast = "γιορτάζει η Κασσιανή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:00 Δύση Ήλιου 18:45"

            ElseIf DayOfYear = 252 Then
                ChurchFeast = "+ ΓΕΝΝΕΣΙΟΝ ΘΕΟΤΟΚΟΥ, Ρούφου μάρτυρος " + vbCrLf + "(Κατάλυσης ιχθύος). "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά του Αναλφαβητισμού."
                SunRise = "Ανατολή Ήλιου 6:01 Δύση Ήλιου 18:44"

            ElseIf DayOfYear = 253 Then
                ChurchFeast = "+ΠΡΟ ΥΨΩΣΕΩΣ Σύναξις Θεοπατόρων Ιωακείμ και" + vbCrLf + "Άννης(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ιωακείμ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:02 Δύση Ήλιου 18:42"

            ElseIf DayOfYear = 254 Then
                ' 10 Σεπτεμβρίου'
                ChurchFeast = "Μηνοδώρας, Μητροδώρας, Νυμφοδώρας, "
                NameFeast = "γιορτάζουν οι: Εράστη, Έραστος, Κλημεντίνη, " + vbCrLf + "Κλήμης, Μητροδώρα."
                WorldDay = " Παγκόσμια Ημέρα κατά της Αυτοκτονίας."
                SunRise = "Ανατολή Ήλιου 6:03 Δύση Ήλιου 18:41"

            ElseIf DayOfYear = 255 Then
                ChurchFeast = "Θεοδώρας, Πάυλου Εφέσ., Ευανθίας μαρτ., "
                NameFeast = "γιορτάζει η Ευανθία"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:04 Δύση Ήλιου 18:39"

            ElseIf DayOfYear = 256 Then
                ChurchFeast = "Αυτονόμου, μάρτ., Δανιήλ του Θασίου, Κουρνούτου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:04 Δύση Ήλιου 18:37"

            ElseIf DayOfYear = 257 Then
                ChurchFeast = "+Κορνηλίου Ιεροθέου, Ιβηρίτου Κρονίδου" + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Αριστείδης, Αριστέας, Άρης, " + vbCrLf + "Αριστέα, Κορνήλιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:05 Δύση Ήλιου 18:36"

            ElseIf DayOfYear = 258 Then
                ChurchFeast = "+ΥΨΩΣΗΣ ΤΟΥ ΤΙΜΙΟΥ ΣΤΑΥΡΟΥ (Νηστεία), "
                NameFeast = "γιορτάζουν οι: Θεοκλής, Σταύρος, Σταυρούλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:06 Δύση Ήλιου 18:34"

            ElseIf DayOfYear = 259 Then
                ChurchFeast = "Νικήτα, Βησσαρίωνος, Νικολάου Κρήτης, "
                NameFeast = "γιορτάζουν οι: Βησσαρίων, Νικήτας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:07 Δύση Ήλιου 18:33"

            ElseIf DayOfYear = 260 Then
                ChurchFeast = "+Ευφημίας μεγαλομάρτυρος, Μελιτίνης μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Ευφημία, Λουντμίλλα. "
                WorldDay = "Παγκόσμια Ημέρα για τη διατήρηση της στοιβάδας" + vbCrLf + "του όζοντος."
                SunRise = "Ανατολή Ήλιου 6:08 Δύση Ήλιου 18:31"

            ElseIf DayOfYear = 261 Then
                ChurchFeast = "+ Σοφίας, Πίστεως, Αγάπης, Ελπίδος μαρτ., "
                NameFeast = "γιορτάζουν οι: Σοφία, Πίστη, Ελπίδα, Αγάπη, " + vbCrLf + "Αγαθοκλής, Σόνια"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:09 Δύση Ήλιου 18:30"

            ElseIf DayOfYear = 262 Then
                ChurchFeast = "Ευμενίου, Κάστορος, Αριάδνης μάρτυρος, "
                NameFeast = "γιορτάζουν οι Ευμένιος, Αριάδνη, Ρωμύλος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:10 Δύση Ήλιου 18:28"

            ElseIf DayOfYear = 263 Then
                ChurchFeast = "Τροφίμου, Σαββατίου, Δορυμέδοντος μαρ., "
                NameFeast = "γιορτάζει ο Σαββάτιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:10 Δύση Ήλιου 18:27"

            ElseIf DayOfYear = 264 Then
                '20 Σεπτεμβρίου'
                ChurchFeast = "+Ευσταθίου μεγαλ., Θεοπίστης, Μαρτίνου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου)"
                NameFeast = "γιορτάζουν οι: Στάθης, Ευσταθία"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:11 Δύση Ήλιου 18:25"

            ElseIf DayOfYear = 265 Then
                ChurchFeast = "+ΜΕΤΑ ΤΗΝ ΥΨΩΣΙΝ, Κορδάτου, Ιωνά, Μελετίου," + vbCrLf + "Πρίσκου μαρτ., "
                NameFeast = ""
                WorldDay = " Διεθνής Ημέρα Ειρήνης, Παγκόσμια Ημέρα Αλτσχάιμερ."
                SunRise = "Ανατολή Ήλιου 6:12 Δύση Ήλιου 18:23"

            ElseIf DayOfYear = 266 Then
                ChurchFeast = "Φωκά ιερομ., Ισαάκ και Μαρτίνου μαρτύρων, "
                NameFeast = "γιορτάζει η Ζωγραφιά. "
                WorldDay = "Ευρωπαϊκή Ημέρα Χωρίς Αυτοκίνητο."
                SunRise = "Ανατολή Ήλιου 6:13 Δύση Ήλιου 18:22"

            ElseIf DayOfYear = 267 Then
                ChurchFeast = "+Σύλληψις Ιωάννου Προδρόμου, Ξανθίππης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ξανθίππη, Ξανθή, Πολυξένη, Ξένη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:14 Δύση Ήλιου 18:20"

            ElseIf DayOfYear = 268 Then
                ChurchFeast = "+ Ανάμνησις Θαύματος Μυρτιδιωτίσσης, "
                NameFeast = " γιορτάζουν οι: Θέκλα, Μυρσίνη, Περσεφόνη"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:15 Δύση Ήλιου 18:19"

            ElseIf DayOfYear = 269 Then
                ChurchFeast = "Ευφοσύνης Παφνουτίου, Τάττης μάρτ. "
                NameFeast = "γιορτάζουν οι: Ευφροσύνη, Φρόσύνη, Φρόσω"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:17"

            ElseIf DayOfYear = 270 Then
                ChurchFeast = "+Μετάστασις Ιωάννου Θεολόγου, Γεδεών " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = " Ευρωπαϊκή Ημέρα Γλωσσών"
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:16"

            ElseIf DayOfYear = 271 Then
                ChurchFeast = "Καλλιστράτου, Αριστάρχου, Γαϊανής, "
                NameFeast = "γιορτάζουν οι Ακυλίνα, Ακυλίνη, Ζήνων. "
                WorldDay = "Παγκόσμια Ημέρα Τουρισμού."
                SunRise = "Ανατολή Ήλιου 6:17 Δύση Ήλιου 18:14"

            ElseIf DayOfYear = 272 Then
                ChurchFeast = "Χαρίτωνος ομολ., Νεοφύτου, Βαρούχ προφ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:12"

            ElseIf DayOfYear = 273 Then
                ChurchFeast = "Κυριακού αναχωρ., Δορυμέδοντος, Δάδα., "
                NameFeast = "γιορτάζει ο Κυριάκος. "
                WorldDay = "Παγκόσμια Ναυτική Ημέρα."
                SunRise = "Ανατολή Ήλιου 6:19 Δύση Ήλιου 18:11"

            ElseIf DayOfYear = 274 Then
                '30 Σεπτεμβρίου'
                ChurchFeast = "Γρηγορίου, Στρατονίκου, Μαδρονίου μάρτ. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Καρδιάς."
                SunRise = "Ανατολή Ήλιου 6:20 Δύση Ήλιου 18:09"

            ElseIf DayOfYear = 275 Then
                '1η Οκτωβρίου'
                ChurchFeast = "Ανανίου αποστόλου, Ρωμανού Μελωδού, "
                NameFeast = "γιορτάζουν οι: Θηρεσία, Ρομανός. "
                WorldDay = "Παγκόσμια Ημέρα Κατοικίας, Παγκόσμια Ημέρα Τρίτης" + vbCrLf + "Ηλικίας, Πάγκόσμια Ημέρα κατά της Ηπατίτιδας C."
                SunRise = "Ανατολή Ήλιου 6:21 Δύση Ήλιου 18:08"

            ElseIf DayOfYear = 276 Then
                ChurchFeast = "Κυπριανου ιερομ., Ιουστίνης μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Ιουστίνη, Κυπριανός."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:22 Δύση Ήλιου 18:06"

            ElseIf DayOfYear = 277 Then
                ChurchFeast = "+Διονυσίου Αεροπαγίτου, Δαμάριδος, "
                NameFeast = "γιορτάζει ο Διονύσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:23 Δύση Ήλιου 18:05"

            ElseIf DayOfYear = 278 Then
                ChurchFeast = "Ιεροθέου επισκ. Αθηνών, Δομνίνης μαρτ., "
                NameFeast = "γιορτάζουν οι: Βρίνα, Βρίνη, Βέρα, Ιερόθεος," + vbCrLf + "Ιεροθέα. "
                WorldDay = "Παγκόσμια Ημέρα των Ζώων."
                SunRise = "Ανατολή Ήλιου 6:24 Δύση Ήλιου 18:03"

            ElseIf DayOfYear = 279 Then
                ChurchFeast = "Χαριτίνη, Ευδοκίμου, Μεθοδιας εν Κιμώλω, "
                NameFeast = "γιορτάζει η Χαριτίνη. "
                WorldDay = "Παγκόσμια Ημέρα Εκπαιδευτικών."
                SunRise = "Ανατολή Ήλιου 6:25 Δύση Ήλιου 18:02"

            ElseIf DayOfYear = 280 Then
                ChurchFeast = "+Θωμά αποστόλου, Ερωτηΐδος μάρτυρος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου)."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:25 Δύση Ήλιου 18:00"

            ElseIf DayOfYear = 281 Then
                ChurchFeast = "Σεργίου και Βάκχου μαρτ., Πολυχρονίου, "
                NameFeast = "γιορτάζουν οι: Βάκχος, Πολυχρόνης, Χρόνης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:26 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 282 Then
                ChurchFeast = "Πελαγίας οσίας, Πελαγίας παρθένου, " + vbCrLf + "Ταϊσίας οσίας, "
                NameFeast = "γιορτάζει η Πελαγία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:27 Δύση Ήλιου 17:57"

            ElseIf DayOfYear = 283 Then
                ChurchFeast = "+ Ιακώβου του Αλφαίου αποστ., Ανδρονίκου " + vbCrLf + "και Αθανασίας, "
                NameFeast = "γιορτάζει ο Αβραάμ. "
                WorldDay = "Παγκόσμια Ημέρα Ταχυδρομείου"
                SunRise = "Ανατολή Ήλιου 6:28 Δύση Ήλιου 17:56"

            ElseIf DayOfYear = 284 Then
                '10 Οκτωβρίου'
                ChurchFeast = "Ευλαμπίου και Ευλαμπίας μαρτ.,Θεοφίλου οσίου, "
                NameFeast = "γιορτάζει ο Ευλάμπιος. "
                WorldDay = "Διεθνής Ημέρα μείωσης των φυσικών Καταστροφών, " + vbCrLf + "Παγκόσμια Ημερα ψυχικής Υγείας," + vbCrLf + "Παγκόσμια Ημέρα κατά της Θανατικής ποινής."
                SunRise = "Ανατολή Ήλιου 6:29 Δύση Ήλιου 17:54"

            ElseIf DayOfYear = 285 Then
                ChurchFeast = "Φιλίππου διακόνου,Θεοφάνους Γραπτού"
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Όρασης (Κατά της Τύφλωσης)."
                SunRise = "Ανατολή Ήλιου 6:30 Δύση Ήλιου 17:53"

            ElseIf DayOfYear = 286 Then
                ChurchFeast = "Πρόβου, Ταράχου, Ανδρονίκου, Συμεών νέου Θεολόγου, "
                NameFeast = "γιορτάζουν οι: Ανδρομάχη, Μάχη, Ανδρόμαχος, Βαλάντιος. "
                WorldDay = "Παγκόσμια Ημέρα Αυγού, Παγκόσμια Ημέρα κατά της " + vbCrLf + "Αρθρίτιδας."
                SunRise = "Ανατολή Ήλιου 6:31 Δύση Ήλιου 17:52"

            ElseIf DayOfYear = 287 Then
                ChurchFeast = "Κάρπου, Παπύλου, Αγαθοδώρου μαρτύρων, "
                NameFeast = "γιορτάζει η Αγαθονίκη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:32 Δύση Ήλιου 17:50"

            ElseIf DayOfYear = 288 Then
                ChurchFeast = "Ναζαρίου, Γερβασίου, Κοσμά Μελωδού, "
                NameFeast = "γιορτάζει ο Γερβάσιος. "
                WorldDay = "Παγκόσμια Ημέρα Προτύπων Τυποποίησης."
                SunRise = "Ανατολή Ήλιου 6:33 Δύση Ήλιου 17:49"

            ElseIf DayOfYear = 289 Then
                ChurchFeast = "Λουκιανού, Σαβίνου, Βάρσου, Ευθυμίου νεομάρτυρος, "
                NameFeast = "γιορτάζει ο Λουκιανός. "
                WorldDay = "Διεθνής Ημέρα του Λευκού Μπαστουνιού."
                SunRise = "Ανατολή Ήλιου 6:34 Δύση Ήλιου 17:47"

            ElseIf DayOfYear = 290 Then
                ChurchFeast = "Λογγίνου εκατοντάρχου, Λεοντίου και των συν αυτώ. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Διατροφής, Παγκόσμια Ημέρα " + vbCrLf + "Σπονδυλικής Στήλης."
                SunRise = "Ανατολή Ήλιου 6:35 Δύση Ήλιου 17:46"

            ElseIf DayOfYear = 291 Then
                ChurchFeast = "Ωσηέ προφήτου, Ανδρέου του εν κρίσει, "
                NameFeast = "γιορτάζουν οι: Αντίγονος, Ευπρέπιος. "
                WorldDay = "Παγκόσμια Ημέρα Για την εξάλειψη της Φτώχειας," + vbCrLf + "Παγκόσμια Ημέρα Μυοσκελετικού τραύματος."
                SunRise = "Ανατολή Ήλιου 6:36 Δύση Ήλιου 17:45"

            ElseIf DayOfYear = 292 Then
                ChurchFeast = "+Λουκά Ευαγγελιστού, Μαρίνου μάρτυρος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Λουκάς Λουκία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:37 Δύση Ήλιου 17:43"

            ElseIf DayOfYear = 293 Then
                ChurchFeast = "Ιωήλ προφήτου, Ουάρου μαρτ., Κλεοπάτρας."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:38 Δύση Ήλιου 17:42"

            ElseIf DayOfYear = 294 Then
                '20 Οκτωβρίου'
                ChurchFeast = "+Γερασίμου Κεφαλληνίας, Αρτεμίου, Μαρτώνης, "
                NameFeast = "γιορτάζουν οι:Αρτέμιος, Αρτέμης, Άρτεμις, " + vbCrLf + "Γεράσιμος, Ενόη. "
                WorldDay = "Παγκόσμια Ημέρα Οστεοπόρωσης."
                SunRise = "Ανατολή Ήλιου 6:39 Δύση Ήλιου 17:40"

            ElseIf DayOfYear = 295 Then
                ChurchFeast = "Χριστοδ.εν Πάτμω, Ιλαρίωνος μεγαλομ., Σωκράτους"
                NameFeast = "γιορτάζουν οι: Ευκράτης, Ούρσουλα, Σωκράτης," + vbCrLf + "Χριστόδουλος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:40 Δύση Ήλιου 17:39"

            ElseIf DayOfYear = 296 Then
                ChurchFeast = "Αβερκιου Ιεραπόλεως, Γλυκερίας, Γαϊου, "
                NameFeast = "γιορτάζουν οι: Αβέρκιος, Αβερκία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:41 Δύση Ήλιου 17:38"

            ElseIf DayOfYear = 297 Then
                ChurchFeast = "+Ιακώβου απ.Αδελφοθέου, Ιγνατίου Κων/λεως " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ιάκωβος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:42 Δύση Ήλιου 17:37"

            ElseIf DayOfYear = 298 Then
                ChurchFeast = "Αρέθα μεγαλομ., Σεβαστιανής μάρτυρος, "
                NameFeast = "γιορτάζει η Σεβαστιανή. "
                WorldDay = "Ημέρα του ΟΗΕ."
                SunRise = "Ανατολή Ήλιου 6:43 Δύση Ήλιου 17:35"

            ElseIf DayOfYear = 299 Then
                ChurchFeast = "Ταβιθάς εν Ιοππη, Μαρκιανού, Μαρτυρίου, "
                NameFeast = "γιορτάζει η Χρυσάνθη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:44 Δύση Ήλιου 17:34"

            ElseIf DayOfYear = 300 Then
                ChurchFeast = "+Δημητρίου Μεγαλομάρτυρος Μυροβλήτου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Δημήτριος, Δήμητρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:45 Δύση Ήλιου 17:33"

            ElseIf DayOfYear = 301 Then
                ChurchFeast = "Νέστορος μεγαλομ., Πρόκλης συζ. Πιλάτου, "
                NameFeast = "γιορτάζει ο Νέστορας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:46 Δύση Ήλιου 17:32"

            ElseIf DayOfYear = 302 Then
                ChurchFeast = "+Αγίας Σκέπης (Επέτειος του ΟΧΙ)."
                NameFeast = ""
                WorldDay = " (Εθνική εορτή)"
                SunRise = "Ανατολή Ήλιου 6:47 Δύση Ήλιου 17:30"

            ElseIf DayOfYear = 303 Then
                ChurchFeast = "Αναστασίας Ρωμαίας, Αβραμίου Μελιτινής. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Ψωρίασης."
                SunRise = "Ανατολή Ήλιου 6:48 Δύση Ήλιου 17:29"

            ElseIf DayOfYear = 304 Then
                '30 Οκτωβρίου
                ChurchFeast = "Κλεόπα, Αρτεμά, Ζηνοβίου και Ζηνοβίας, "
                NameFeast = "γιορτάζουν οι: Ζηνώβιος, Ζηνωβια, Απολλωνία," + vbCrLf + "Αστέριος, Αστέρης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:49 Δύση Ήλιου 17:28"

            ElseIf DayOfYear = 305 Then
                ChurchFeast = "Στάχυος, Απελλού, Αμπλία, Αριστοβούλου, "
                NameFeast = "γιορτάζουν οι: Απελλής, Απέλλης, Αριστόβουλος, " + vbCrLf + "Αριστοβούλη."
                WorldDay = "Παγκόσμια Ημέρα Αποταμίευσης."
                SunRise = "Ανατολή Ήλιου 6:50 Δύση Ήλιου 17:27"

            ElseIf DayOfYear = 306 Then
                '1η Νοεμβρίου
                ChurchFeast = "+Αγίων Αναργύρων κοσμα και Δαμιανού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάουν οι: Ανάργυρος, Ανάργυρη, Κόσμας, " + vbCrLf + "Δαμιανός. "
                WorldDay = "Παγκόσμια Ημέρα Χορτοφαγίας."
                SunRise = "Ανατολή Ήλιου 6:51 Δύση Ήλιου 17:26"

            ElseIf DayOfYear = 307 Then
                ChurchFeast = "Ακινδύνου, Πηγασίου, Ελπιδοφόρου μάρτυρως, "
                NameFeast = "γιορτάζουν οι: Αφθόνιος, Αφθονία, Ελπιδοφόρος," + vbCrLf + "Πήγασος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:52 Δύση Ήλιου 17:25"

            ElseIf DayOfYear = 308 Then
                ChurchFeast = "Ακεψιμά, Αειθαλά, Ιωσήφ Μαρτύρων."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:53 Δύση Ήλιου 17:24"

            ElseIf DayOfYear = 309 Then
                ChurchFeast = "Ιωαννικίου οσίου, Νικάνδρου, Ερμαίου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:54 Δύση Ήλιου 17:23"

            ElseIf DayOfYear = 310 Then
                ChurchFeast = "Γαλακτίωνος, Επιστήμης μαρτυρος."
                NameFeast = "γιορτάζουν οι: Γαλακτίων, Λίνος, Λίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:55 Δύση Ήλιου 17:22"

            ElseIf DayOfYear = 311 Then
                ChurchFeast = "Πάυλου κων/λεως, λουκα οσίου, Δημητριανού " + vbCrLf + "Κύπρου, "
                NameFeast = "γιορτάζει ο Λεονάρδος."
                WorldDay = "Παγκόσμια Ημέρα κατά της εκμετάλλευσης του " + vbCrLf + "περιβάλλοντος στον πόλεμο και τις Ένοπλες Συγκρούσης."
                SunRise = "Ανατολή Ήλιου 6:56 Δύση Ήλιου 17:21"

            ElseIf DayOfYear = 312 Then
                ChurchFeast = "Των εν Μελιτινή 33 μαρτ., Λαζάρου οσίου, "
                NameFeast = "γιορτάζουν οι: Αθηνόδωρος, Θεαγένης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:57 Δύση Ήλιου 17:20"

            ElseIf DayOfYear = 313 Then
                ChurchFeast = "+Παμεγγίστων Ταξιαρχών Μιχαήλ και Γαβριήλ " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Μιχάλης, Άγγελος, Αγγελικη, " + vbCrLf + "Ταξιάρχης, Γαβριήλ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:58 Δύση Ήλιου 17:19"

            ElseIf DayOfYear = 314 Then
                ChurchFeast = "+Ονησιφόρου μαρτ., Νεκταρίου εν Αιγίνη, "
                NameFeast = "γιορτάζουν οι: Νεκτάριος, Νεκταρία, Ελλάδιος. "
                WorldDay = "Διεθνής Ημέρα κατά του Φασισμύ και του" + vbCrLf + "Αντισημιτισμου."
                SunRise = "Ανατολή Ήλιου 7:00 Δύση Ήλιου 17:18"

            ElseIf DayOfYear = 315 Then
                ' 10 Νοεμβρίου
                ChurchFeast = "Ολυμπά, Σωσιπάτρου, Ροδίωνος, Ορέστου, "
                NameFeast = "γιορτάζουν οι: Αρσένιος, Αρσένης, Αρσενία, " + vbCrLf + "Αρσιόνη, Ωρίων."
                WorldDay = "Παγκόσμια Ημέρα Επιστήμης για την Ειρήνη και " + vbCrLf + "την Ανάπτυξη."
                SunRise = "Ανατολή Ήλιου 7:01 Δύση Ήλιου 17:17"

            ElseIf DayOfYear = 316 Then
                ChurchFeast = "+Μηνά μεγαλομ., Βίκτωρος και Βικεντίου, "
                NameFeast = "γιορτάζουν οι: Μηνάς, Βικέντιος, Βίκτωρ, " + vbCrLf + "Βικτόρια, Δράκων."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:02 Δύση Ήλιου 17:16"

            ElseIf DayOfYear = 317 Then
                ChurchFeast = "+Ιωάννου Ελεήμονος, Νείλου οσίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου)."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:03 Δύση Ήλιου 17:15"

            ElseIf DayOfYear = 318 Then
                ChurchFeast = "+Ιωάννου Χρυσοστόμου, Δαμασκινού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Χρυσόστομος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:04 Δύση Ήλιου 17:15"

            ElseIf DayOfYear = 319 Then
                ChurchFeast = "+Φιλίππου αποστόλου, Κων/νου Υδραίου," + vbCrLf + "Γρ. Παλαμά (Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζει ο Φίλιππος. "
                WorldDay = "Παγκόσμια Ημέρα κατά του Διαβήτη."
                SunRise = "Ανατολή Ήλιου 7:05 Δύση Ήλιου 17:14"

            ElseIf DayOfYear = 320 Then
                ChurchFeast = "Γουρία, Σαμμωνά, Αβίβου μαρτ." + vbCrLf + "(αρχή νηστίας.) "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Φιλοσοφίας."
                SunRise = "Ανατολή Ήλιου 7:06 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 321 Then
                ChurchFeast = "+Ματθαίου αποστόλου και Ευαγγελιστού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ματθαίος, Ιφιγένια, Ματθίλντη. "
                WorldDay = "Διεθνής Ημέρα Ανεκτικότητας."
                SunRise = "Ανατολή Ήλιου 7:07 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 322 Then
                ChurchFeast = "Γριγορίου Νεοκαισαρείας, Γενναδίου Κωνστ., "
                NameFeast = "γιορτάζει ο Γεννάδιος. "
                WorldDay = "Διεθνής Ημέρα Σπουδαστών."
                SunRise = "Ανατολή Ήλιου 7:08 Δύση Ήλιου 17:12"

            ElseIf DayOfYear = 323 Then
                ChurchFeast = "Πλάτωνος-Ρωμανού, Ζακχαίου μαρτύρων, "
                NameFeast = "γιορτάζει ο Πλάτων."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:09 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 324 Then
                ChurchFeast = "Αβδιού προφήτου, Βαρλαάμ μάρτυρος. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Δημόσιας Τουαλέτας."
                SunRise = "Ανατολή Ήλιου 7:10 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 325 Then
                '20 Νοεμβρίου
                ChurchFeast = "Γριγορ. Δεκαπολίτου, Πρόκλου Κωνστ/λεως, "
                NameFeast = "γιορτάζουν οι: Δεναχίς, Δεναχίδα. "
                WorldDay = "Ημέρα για την Εκβιομηχάνιση της Αφρικής, " + vbCrLf + "Παγκόσμια Ημέρα για τα δικαιώματα του Παιδιού."
                SunRise = "Ανατολή Ήλιου 7:11 Δύση Ήλιου 17:10"

            ElseIf DayOfYear = 326 Then
                ChurchFeast = "+ΤΑ ΕΙΣΟΔΙΑ ΤΗΣ ΥΠΕΡΑΓΙΑΣ ΘΕΟΤΟΚΟΥ (Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν οι: Μαρία - για ανύπανρες, Μάριος, Σουλτάνα. "
                WorldDay = "Παγκόσμια Ημέρα Χρόνιας Αποφρακτικής Πνευμονοπάθειας, " + vbCrLf + "Παγκόσμια Ημέρα Τηλεόρασης, Παγκόσμια Ημέρα Χαιρετισμού."
                SunRise = "Ανατολή Ήλιου 7:12 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 327 Then
                ChurchFeast = "φιλήμονος αποστόλου και των συν αυτώ, "
                NameFeast = "γιορτάζουν οι: Βαλεριανός, Βαλεριάνα, Βαλερία, " + vbCrLf + "Βαλέριος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:13 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 328 Then
                ChurchFeast = "Αμφιλοχίου επισκοπ.Ικονίου, Σισινίου ομολογ., "
                NameFeast = "γιορτάζουν οι: Μερόπη, Έλενος, Λένος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:14 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 329 Then
                ChurchFeast = "Κλήμεντος, Πέτρου Αλεξ/νδρ., Ερμογένους, "
                NameFeast = "γιορτάζει η Φλώρα. "
                WorldDay = "Παγκόσμια Ημέρα Αγοραστικής Αποχής."
                SunRise = "Ανατολή Ήλιου 7:15 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 330 Then
                ChurchFeast = "+Αικατερίνης μεγαλομ., Μερκουρίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι : Κατερίνα, Αικατερίνη, Κατίνα," + vbCrLf + "Κυπαρισσία, Σίσσυ. "
                WorldDay = " Διεθνής Ημέρα κατά της κακοποίησης της Γυναίκας."
                SunRise = "Ανατολή Ήλιου 7:16 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 331 Then
                ChurchFeast = "+Στυλιανού Παφλαγ., Νίκωνος (Μετανοείτε), "
                NameFeast = "γιορτάζουν οι: Στέλιος Στέλλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:17 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 332 Then
                ChurchFeast = "Ιακώβου Πέρσου, Ναθαναήλ οσίου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:18 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 333 Then
                ChurchFeast = "Στεφάνου ομολογ., Ειρηνάρχου μάρτύρων, "
                NameFeast = "γιορτάζει ο Ειρήναρχος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:19 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 334 Then
                ChurchFeast = "Παραμόνου, Φαίδρου και 360 μαρτύρων, "
                NameFeast = "γιορτάζει η Φαίδρα. "
                WorldDay = "Διεθνής Ημέρα αλληλεγγύης προς τον " + vbCrLf + "Παλαιστινιακό Λαό."
                SunRise = "Ανατολή Ήλιου 7:20 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 335 Then
                '30 Νοεμβρίου
                ChurchFeast = "+Ανδρέου πρωτοκλήτου, Φρουμεντίου Ινδίας " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = " γιορτάζουν οι: Ανδρέας, Ανδρίκος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:21 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 336 Then
                '1η Δεκεβμρίου
                ChurchFeast = "Ναούμ προφήτου, Θεοκλήτου, Φιλαρέτου, "
                NameFeast = "γιορτάζουν οι: Ναούμ, Φιλάρετος, Ιακώβ. "
                WorldDay = "Παγκόσμια Ημέρα κατά του AIDS."
                SunRise = "Ανατολή Ήλιου 7:22 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 337 Then
                ChurchFeast = "Αββακούμ προφ., Μυρόπης, Θεοφίλου, "
                NameFeast = "γιορτάζουν οι: Μερόπη, Σολομών. "
                WorldDay = "Παγκόσμια Ημέρα για την εξάλειψη της Δουλείας."
                SunRise = "Ανατολή Ήλιου 7:23 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 338 Then
                ChurchFeast = "Σοφονίου προφ., Θεοδούλα οσ., Αγγελή, "
                NameFeast = "γιορτάζει ο Γλυκέριος. "
                WorldDay = "Παγκόσμια Ημέρα Ατόμων με Ειδικές Ανάγκες."
                SunRise = "Ανατολή Ήλιου 7:24 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 339 Then
                ChurchFeast = "+Βαρβάρας μεγαλ.,Ιωάννου Δαμασκηνού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βαρβάρα, Δαμασκηνός, Σεραφείμ."
                WorldDay = "Παγκόσμια Ημέρα κατά των Ναρκων."
                SunRise = "Ανατολή Ήλιου 7:25 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 340 Then
                ChurchFeast = "+Σάββα Ηγιασμένου, Διογένους, Νίνου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Διογένης, Σάββας. "
                WorldDay = "Παγκόσμια Ημέρα Εθελοντισμού για την " + vbCrLf + "Οικονομική και Κοινωνική Ανάπτυξη."
                SunRise = "Ανατολή Ήλιου 7:26 Δύση Ήλιου 17:05"

            ElseIf DayOfYear = 341 Then
                ChurchFeast = "+Νικολάου Μύρων, Νικολάου νεομάρτυρ " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Νίκος, Νικολέτα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:27 Δύση Ήλιου 17:05"

            ElseIf DayOfYear = 342 Then
                ChurchFeast = "Αμβροσίου Μεδιαλάνων, Νεοφύτου μαρτ., "
                NameFeast = "γιορτάζουν  οι: Αμβρόσιος, Αμβρόσης, Αμβροσία. "
                WorldDay = "Διεθνής Ημέρα Πολιτικής Αεροπορίας."
                SunRise = "Ανατολή Ήλιου 7:28 Δύση Ήλιου 17:05"

            ElseIf DayOfYear = 343 Then
                ChurchFeast = "+Παταπίου οσίου, Σωφρονίου, Σωσθένους, "
                NameFeast = "γιορτάζει ο Πατάπιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:29 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 344 Then
                ChurchFeast = "+Σύλληψις. Αγ. Άννης μητρός της Θεοτόκου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = "Γιορτάζουν οι: Άννα, Αννούλα, Αννίτα, Αννέτα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:30 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 345 Then
                '10 Δεκεμβρίου
                ChurchFeast = "Μηνά, Ερμογένους, Ευγράφου, Θεοτέκν."
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Ανθρωπίνων Δικαιωμάτων, " + vbCrLf + "Παγκόσμια Ημέρα Ιδιοκτησίας"
                SunRise = "Ανατολή Ήλιου 7:30 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 346 Then
                ChurchFeast = "Δανιήλ Στυλίτου, Αιμιλιανού, Βεβαίας μαρ., "
                NameFeast = "γιορτάζουν οι: Αρών, Αδάμ, Αδαμάντιος," + vbCrLf + "Αδαμαντία, Δανάη. "
                WorldDay = "Παγκόσμια Ημέρα Βουνού."
                SunRise = "Ανατολή Ήλιου 7:31 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 347 Then
                ChurchFeast = "+Σπυρίδωνος Τριμυθούντος του θαυμ., Αλεξάνδρου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Σπύρος."
                WorldDay = "Διεθνής Ημέρα Παιδικής Τηλεόρασης."
                SunRise = "Ανατολή Ήλιου 7:32 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 348 Then
                ChurchFeast = "Ευγενίου, Λουκίας, Ευστρατίου, Αυξεντ., "
                NameFeast = "γιορτάζουν οι: Άρης, Στρατής, Στρατούλα, Λουκία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 349 Then
                ChurchFeast = "Θύρσου, Απολλωνίου, Καλλινίκ., Λευκίου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 350 Then
                ChurchFeast = "+Ελευθερίου ιερομάρτ., Ανθίας, Βάκχου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ελευθέριος, Ελευθερία, Ανθή," + vbCrLf + "Ανθούλα, Σύλβια."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 351 Then
                ChurchFeast = "Αγγαίου, Μοδέστρου, Θεοφάνους,Πρόβου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 352 Then
                ChurchFeast = "+Διονυσίου Αιγίνης, Δανιήλ προφ., Ιάκχου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Δανιήλ, Διονύσιος, Ιακχος, " + vbCrLf + "Ρεβέκκα, Ολυμπία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 353 Then
                ChurchFeast = "Νικοστράτου, Σεβαστιανού, Κάστορος, "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Μετανάστη."
                SunRise = "Ανατολή Ήλιου 7:36 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 354 Then
                ChurchFeast = "Βονιφατίου, Ευτυχίου, Αγλαϊδος, Άρεως"
                NameFeast = "γιορτάζει η Αγλαΐα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:36 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 355 Then
                '20 Δεκεμβρίου
                ChurchFeast = "+Ιγνατίου Θεοφόρου, φιλογονίου, Ιωάννου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ιγνάτιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:37 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 356 Then
                ChurchFeast = "Ιουλιανής, Θεμιστοκλέους, Πέτρου, "
                NameFeast = "γιορτάζουν οι: Θεμιστοκλης, Θέμης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 357 Then
                ChurchFeast = "Αναστασίας φαρμακ., Χρυσογόνου, "
                NameFeast = "γιορτάζει η Αναστασία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:10"

            ElseIf DayOfYear = 358 Then
                ChurchFeast = "+ΠΡΟ ΧΡΙΣΤΟΥ ΓΕΝΝ., Εν. κρήτη 10 μαρτ."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:10"

            ElseIf DayOfYear = 359 Then
                ChurchFeast = "Παραμ. Χριστουγέννων, Ευγενίας, Υακίνθου, "
                NameFeast = "γιορτάζουν οι: Ευγενία, Ευγένιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 360 Then
                ChurchFeast = "+Η ΚΑΤΑ ΣΑΡΚΑ ΓΕΝΝΗΣΙΣ ΤΟΥ ΚΥΡΙΟΥ ΚΑΙ ΣΩΤΗΡΟΣ " + vbCrLf + "ΗΜΩΝ ΙΗΣΟΥ ΧΡΙΣΤΟΥ, "
                NameFeast = "γιορτλάζουν οι: Χρήστος, Χρηστίνα, Χρύσα, Μανώλης, " + vbCrLf + "Εμμανουέλλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 361 Then
                ChurchFeast = "+Η σύναξις Της Θεοτόκου, Ευθυμίου, "
                NameFeast = "γιορτάζει ο Δαβίδ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:12"

            ElseIf DayOfYear = 362 Then
                ChurchFeast = "+Στεφάνου Πρωτομάρτ., Θεοδώρου, "
                NameFeast = "γιορτάζει ο Στέφανος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 363 Then
                ChurchFeast = "Των εν Νικομηδεία Δισμυρίων Μαρτύρων, "
                NameFeast = "γιορτάζει η Δόμνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 364 Then
                ChurchFeast = "Των Υπό Ηρώδου Αναιρεθέντων Νηπίων, "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα για τη Βιοποικιλότητα."
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:14"

            ElseIf DayOfYear = 365 Then
                '30 Δεκεμβρίου
                ChurchFeast = "+ΜΕΤΑ ΧΡΙΣΤΟΥ ΓΕΝΝ., Ανυσίας, Φιλεταίρ, "
                NameFeast = "γιορτάζουν οι: Ανύσιος, Ιωσήφ, Σήφης, Ιωσηφίνα," + vbCrLf + "Ζοζεφίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:15"

            ElseIf DayOfYear = 366 Then
                ChurchFeast = "Μελάνης οσίας, Ζωτικού Ορφανοτροφου, "
                NameFeast = "γιορτάζουν οι: Μελανία, Μέλανυ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:16"
            End If
        Else
            If DayOfYear = 1 Then
                '1η Ιανουαρίου
                ChurchFeast = "+ περιτομή Ιησού Χριστού, Βασιλίου του Μέγα, "
                NameFeast = "γιορτάζουν οι: Βασίλειος, Βασιλική, Βιβή, Βιβιαν," + vbCrLf + "Βίκυ,Τηλέμαχος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:16"

            ElseIf DayOfYear = 2 Then
                ChurchFeast = "Σιλβέστρου ρώμης, Θεογένους ιερομ. "
                NameFeast = "Γιορτάζουν οι: Θεόπεμπτος, Σίλβεστρος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:17"

            ElseIf DayOfYear = 3 Then
                ChurchFeast = "Μαλαχίου προφ, Γορδίου μάρτ. "
                NameFeast = "Γιορτάζει η Γενοβέφα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:18"

            ElseIf DayOfYear = 4 Then
                ChurchFeast = "Θεοκτίστου, Ουνουφίου εν Χίω. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:19"

            ElseIf DayOfYear = 5 Then
                ChurchFeast = "Θεοπέμπτου, Θεώνα, Συγκλητικίς οσίου, "
                NameFeast = "γιορτάζουν οι: Θεωνάς, Θεώνη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:42 Δύση Ήλιου 17:19"

            ElseIf DayOfYear = 6 Then
                ChurchFeast = "+ΤΑ ΑΓΙΑ ΘΕΟΦΑΝΕΙΑ, "
                NameFeast = "γιορτάζουν οι: Φώτης, Θεοφάνης, Φωτεινή," + vbCrLf + "Θεοπούλα, Ιορδάνης, Ουρανία, Περιστέρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:42 Δύση Ήλιου 17:20"

            ElseIf DayOfYear = 7 Then
                ChurchFeast = "+Σύναξις Ιωαννου του Προδρόμου Βαπτιστού" + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν οι: Γιάννης, Γιάννα, Ζαννέτα, " + vbCrLf + "Ζανέτ, Πρόδρομος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:42 Δύση Ήλιου 17:21"

            ElseIf DayOfYear = 8 Then
                ChurchFeast = "Γεωργίου Χοζεβίτου-Δομνίκης-Κύρου, "
                NameFeast = "γιορτάζουν οι: Αγάθων, Δομνίκη, Κέλσιος, Παρθένα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:22"

            ElseIf DayOfYear = 9 Then
                ChurchFeast = "Πολυεύκτου Μάρτυρος, Ευστρατίου οσίου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:23"

            ElseIf DayOfYear = 10 Then
                ChurchFeast = "+ Γρηγορ.Νύσσης, Δομετιανού, Μελιτινής, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:24"

            ElseIf DayOfYear = 11 Then
                ChurchFeast = "Θεοδοσίου, αγαπίου, Μιχαήλ Θαυματ. "
                NameFeast = "Γιορτάζει ο Θεοδόσης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:25"

            ElseIf DayOfYear = 12 Then
                ChurchFeast = "Τατιανής μαρ.,Μερτίου,Ευθασίας, Σάββα. "
                NameFeast = "γιορτάζει η Τατιάνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:26"

            ElseIf DayOfYear = 13 Then
                ChurchFeast = "Ερμύλου και Στρατονίκου, Παχομίου οσίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:27"

            ElseIf DayOfYear = 14 Then
                ChurchFeast = "Απόδ.Θεοφανείων, Εν σινά και Ραϊθό Πατέρ. "
                NameFeast = "Γιορτάζει η Νίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:28"

            ElseIf DayOfYear = 15 Then
                ChurchFeast = "Παύλου, Ιωάννου Καλυβίτου, Πονσουφίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:29"

            ElseIf DayOfYear = 16 Then
                ChurchFeast = "+Προσκ. Αλύσ. Απ.Πέτρου,Δαμασκηνού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:30"

            ElseIf DayOfYear = 17 Then
                ChurchFeast = "+ Αντωνίου Μεγ.-Γεωργίου Ιωανν. Αχιλλά," + vbCrLf + "(Κατάλυσης οίνου και ελαίου) "
                NameFeast = "γιορτάζουν οι: Αντώνης, Αντωνία, Νάκος, " + vbCrLf + "Θεοδόσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:31"

            ElseIf DayOfYear = 18 Then
                ChurchFeast = "+ Αθανασίου και Κυρίλλου Αλεξανδρείας. " + vbCrLf + "(Κατάλυσης οίνου και ελαίου) "
                NameFeast = "γιορτάζουν οι: Αθανασία, Θανάσης, Νάνσυ, " + vbCrLf + "Κύριλλος, Θεόδουλος, Θεοδούλη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:32"

            ElseIf DayOfYear = 19 Then
                ChurchFeast = "Μακαρίου Αιγυπτίου, Μάρκου, Αρσενίου, "
                NameFeast = "γιορτάζει ο Μακάριος, Ευφρασία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:33"

            ElseIf DayOfYear = 20 Then
                ChurchFeast = "+ Ευθυμίου Μεγ.-Ζαχαρίου-Ευσεβίου, " + vbCrLf + "(Κατάλυσης οίνου και ελαίου) "
                NameFeast = "γιορτάζουν οι: Ευθύμιος, Ευθυμία, Θέμης, " + vbCrLf + "Φαβαινός, Φαβαινή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:34"

            ElseIf DayOfYear = 21 Then
                ChurchFeast = "Μαξίμου Ομολ., Αγνής, Νεοφύτου, Ακύλα, "
                NameFeast = "γιορτάζουν οι: Αγνή, αγνούλα, Ευγένιος, " + vbCrLf + "Πάτροκλος, Μάξιμος, Νεόφυτος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:35"

            ElseIf DayOfYear = 22 Then
                ChurchFeast = "+Τιμοθέου Απ., Αναστ.Πέρσου, Ιωσήφ " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Τιμόθεος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:37 Δύση Ήλιου 17:36"

            ElseIf DayOfYear = 23 Then
                ChurchFeast = "Κλήμεντος, Αγαθαγγέλου, Διονυσίου, "
                NameFeast = "γιορτάζουν οι: Αγαθάγγελος, Αγαθαγγέλα, " + vbCrLf + "Αγαθαγγέλη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:37 Δύση Ήλιου 17:37"

            ElseIf DayOfYear = 24 Then
                ChurchFeast = "Ξένης οσίας, Βαβύλα, Ζωσιμά, Φίλωνος, "
                NameFeast = "γιορτάζουν οι: Ζωσιμάς, Ζωσιμίνα, Ξένη, Ξένια," + vbCrLf + "Φίλωνας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:36 Δύση Ήλιου 17:39"

            ElseIf DayOfYear = 25 Then
                ChurchFeast = "+Γρηγορίου Θεολόγου, Μαργαρίτας " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Γρηγόρης, Γρηγορία, Μαργαρίτα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:40"

            ElseIf DayOfYear = 26 Then
                ChurchFeast = "Ξενοφώντος-Κλήμεντος-Συμεών-Μαρίας, "
                NameFeast = "γιορτάζει ο Ξενοφώντας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:41"

            ElseIf DayOfYear = 27 Then
                ChurchFeast = "+ Ανακ. Λειψ. Ιωάν. Χρυσοστ. Μαρκιανής" + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Χρυσόστομος. "
                WorldDay = "Παγκόσμια Ημέρα κατά της Λέπρας, Διεθνής " + vbCrLf + "Ημέρα Μνήμης για τα θύματα του Ολοκαυτώματος."
                SunRise = "Ανατολή Ήλιου 7:34 Δύση Ήλιου 17:42"

            ElseIf DayOfYear = 28 Then
                ChurchFeast = "Εφραίμ του σύρου, Χάριτος, Παλλαδίου, Ιακώβου, "
                NameFeast = "Σήμερα γιορτάζουν οι: Παλάδιος, Χάρης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:43"

            ElseIf DayOfYear = 29 Then
                ChurchFeast = "+ Ανακ. Λειψ. Ιγν. Θεοφόρ., Παρηγορίου, "
                NameFeast = "γιορτάζουν οι: Βαρσαμία, Βαρσάμω."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:44"

            ElseIf DayOfYear = 30 Then
                ChurchFeast = "+Τριών Ιεραρχών, Εύρ. Εικ. Ευαγγ. Τήνου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Μαύρος, Μαυρουδής, Χρυσή."
                WorldDay = "Σχολική εορτή"
                SunRise = "Ανατολή Ήλιου 7:32 Δύση Ήλιου 17:45"

            ElseIf DayOfYear = 31 Then
                ChurchFeast = "Κύρου και Ιωάννου Αν., Αρσενίου εν Πάρω, "
                NameFeast = "γιορτάζουν οι: Ευδοξία, Κύρος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:31 Δύση Ήλιου 17:46"

            ElseIf DayOfYear = 32 Then
                '1η Φεβρουάριου
                ChurchFeast = "Τρύφωνος, Περπέτουας, Αναστασίου, "
                NameFeast = "γιορτάζουν οι: Τρύφωνας, Φιλικητάτη, Φιλικήτη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:30 Δύση Ήλιου 17:48"

            ElseIf DayOfYear = 33 Then
                ChurchFeast = "+ Η ΥΠΑΠΑΝΤΙ Ι. ΧΡΙΣΤΟΥ, Ιορδάνου " + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζει η Υπαπαντή. "
                WorldDay = "Παγκόσμια Ημέρα Υγροτόπων."
                SunRise = "Ανατολή Ήλιου 7:29 Δύση Ήλιου 17:49"

            ElseIf DayOfYear = 34 Then
                ChurchFeast = "Συμεών Θεοδόχου, Άννης, Σταματίου, "
                NameFeast = "γιορτάζουν οι: Συμεών, Ασημάκης, Ασημίνα, " + vbCrLf + "Σταμάτης, Μαλαματή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:29 Δύση Ήλιου 17:50"

            ElseIf DayOfYear = 35 Then
                ChurchFeast = "Ισιδώρου. Πηλουσιώτου, Νικολάου ομολογητού, "
                NameFeast = "γιορτάζουν οι: Ιάσιμος, Ιασίμη, Σίμος, " + vbCrLf + "Ισίδωρος, Ισιδώρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:28 Δύση Ήλιου 17:51"

            ElseIf DayOfYear = 36 Then
                ChurchFeast = "Αγάθης, Θεοδούλης μάρτ., Πολυεύκτου, "
                NameFeast = "γιορτάζουν οι: Αγαθή, Αγαθούλα, Αγαθώ, Αγαθίτσα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:27 Δύση Ήλιου 17:52"

            ElseIf DayOfYear = 37 Then
                ChurchFeast = "+ Φωτίου κων/πόλεως, Βουκόλου, Ιωσήφ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:26 Δύση Ήλιου 17:53"

            ElseIf DayOfYear = 38 Then
                ChurchFeast = "Παρθενίου Λαμψάκου, Λουκά οσίου, "
                NameFeast = "γιορτάζει ο Παρθένιος. "
                WorldDay = "Παγκόσμια Ημέρα Ασφαλούς Πλοήγησης στο Διαδίκτυο."
                SunRise = "Ανατολή Ήλιου 7:25 Δύση Ήλιου 17:54"

            ElseIf DayOfYear = 39 Then
                ChurchFeast = "+Θεοδώρου Στρατηλάτου, Ζαχαρίου Προφήτου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ζαχαρίας, Ζαχαρούλα, Θεόδωρος," + vbCrLf + "Θεοδώρα, Μάρθα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:24 Δύση Ήλιου 17:55"

            ElseIf DayOfYear = 40 Then
                ChurchFeast = "Νικηφόρου μάρτ., Μαρκέλλου, Φιλαγρίου, "
                NameFeast = "γιορτάζουν οι: Νικηφόρος, Νικηφορία, Μαρκέλος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:23 Δύση Ήλιου 17:56"

            ElseIf DayOfYear = 41 Then
                '10 Φεβρουαρίου
                ChurchFeast = "+Χαράλαμπος ιερομ, Ζήνωνος ταχυδρόμου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Χαράλαμπος, Χαρίλαος, Χαρούλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:22 Δύση Ήλιου 17:58"

            ElseIf DayOfYear = 42 Then
                ChurchFeast = "+Βλασίου ιερομ, Θεοδώρας Αθγούστης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Αυγή, Βλάσιος, Βλάσης, " + vbCrLf + "Θεοδώρα, Δώρα. "
                WorldDay = "Παγκόσμια Ημέρα Του Γάμου."
                SunRise = "Ανατολή Ήλιου 7:21 Δύση Ήλιου 17:59"

            ElseIf DayOfYear = 43 Then
                ChurchFeast = "Μελετίου Αντιοχείας, Αντωνίου Κωνσ/λεως, "
                NameFeast = "γιορτάζουν οι: Μελέτιος, Μελέτης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:19 Δύση Ήλιου 18:00"

            ElseIf DayOfYear = 44 Then
                ChurchFeast = "Ακύλα, Πρισκίλλης αποστ., Μαρτινιανού, "
                NameFeast = "γιορτάζουν: Ακύλας, Πρίσκιλλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:18 Δύση Ήλιου 18:01"

            ElseIf DayOfYear = 45 Then
                ChurchFeast = "Αυξεντίου, Μάρωνος, Γεωργίου Μυτιλήνης, "
                NameFeast = "γιορτάζουν οι: Λουκάς, Βαλεντίνος, Βαλεντίνη. "
                WorldDay = "Ημέρα των ερωτευμένων Καθολικών Χριστιανών."
                SunRise = "Ανατολή Ήλιου 7:17 Δύση Ήλιου 18:02"

            ElseIf DayOfYear = 46 Then
                ChurchFeast = "Ονησίμου αποστόλου, Ευσεβίου, Ανθίμου, "
                NameFeast = "γιορτάζουν οι: Ευσέβιος, Ευσεβής, Ευσεβεία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:16 Δύση Ήλιου 18:03"

            ElseIf DayOfYear = 47 Then
                ChurchFeast = "Ουάλεντος, Φλαβιανού, Παμφίλου μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:15 Δύση Ήλιου 18:04"

            ElseIf DayOfYear = 48 Then
                ChurchFeast = "+Θεοδώρου Τύρωνος, Πουλχερίας, Μαρκιανού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:14 Δύση Ήλιου 18:05"

            ElseIf DayOfYear = 49 Then
                ChurchFeast = "Λέοντος Ρώμης, Αγαπητού Επισκόπου Σιναίου, "
                NameFeast = "γιορτάζουν οι: Αγαπητός, Λέων."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:12 Δύση Ήλιου 18:06"

            ElseIf DayOfYear = 50 Then
                ChurchFeast = "Φιλοθέης Αθηναίας, Φιλήμονος, Κόνωνος οσίου, "
                NameFeast = "γιορτάζουν οι: φιλοθέη, Χλόη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:11 Δύση Ήλιου 18:07"

            ElseIf DayOfYear = 51 Then
                '20 Φεβρουαρίου
                ChurchFeast = "Λέοντος Κατάνης, Αγάθωνος Ρώμης, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:10 Δύση Ήλιου 18:08"

            ElseIf DayOfYear = 52 Then
                ChurchFeast = "Τιμοθέου εν Συμβ., Ευσταθίου Αντιοχείας, "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Μητρικής Γλώσσας, Παγκόσμια " + vbCrLf + "Ημέρα Του Ξεναγού."
                SunRise = "Ανατολή Ήλιου 7:09 Δύση Ήλιου 18:10"

            ElseIf DayOfYear = 53 Then
                ChurchFeast = "Των εν τοις Ευγενίου μαρτ. Θαλασσίου, "
                NameFeast = "γιορτάζουν οι: Ανθούσα, Ανθούση, Θαλάσσιος, " + vbCrLf + "Θάλασσα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:07 Δύση Ήλιου 18:11"

            ElseIf DayOfYear = 54 Then
                ChurchFeast = "Πολυκάρπου Σμύρνης, Προτερίου ιερομάρτυρος, "
                NameFeast = "γιορτάζει ο Πολύκαρπος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:06 Δύση Ήλιου 18:12"

            ElseIf DayOfYear = 55 Then
                ChurchFeast = " +Α' και Β' εύρεσης Τιμίας κεφαλής Προδρόμου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:05 Δύση Ήλιου 18:13"

            ElseIf DayOfYear = 56 Then
                ChurchFeast = "Ταρασίου Κων/λεως, Ρηγίνου Σκοπέλου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:03 Δύση Ήλιου 18:13"

            ElseIf DayOfYear = 57 Then
                ChurchFeast = "Φωτεινής Σαμαρείτιδος, Πορφυρίου Γάζης, "
                NameFeast = "γιορτάζουν οι: Ανατολή, Φωτεινή, Πορφύριος, Σεβαστιανός."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:03 Δύση Ήλιου 18:14"

            ElseIf DayOfYear = 58 Then
                ChurchFeast = "Προκοπίου Δακαπολίτου, Στεφάνου, "
                NameFeast = "γιορτάζουν οι: Ασκληπιός, Νήσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:02 Δύση Ήλιου 18:15"

            ElseIf DayOfYear = 59 Then
                ' 28 Φεβρουαρίου
                ChurchFeast = "Βασιλείου ομολ.-Κυράννης, Κασσιανού, "
                NameFeast = "γιορτάζει η Μαριάννα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:01 Δύση Ήλιου 18:16"

            ElseIf DayOfYear = 60 Then
                '1η Μαρτίου
                ChurchFeast = "Ευδοκίας, Αντωνίνης, Μαρκέλου μαρτ. "
                NameFeast = " Γιορτάζουν οι: Ευδοκία, Παρασκευάς."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:58 Δύση Ήλιου 18:18"

            ElseIf DayOfYear = 61 Then
                ChurchFeast = "Νικολάου Πλανά Ναξίου, Ησυχίου, Ευθαλίας, "
                NameFeast = "γιορτάζει η Ευθαλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:55 Δύση Ήλιου 18:20"

            ElseIf DayOfYear = 62 Then
                ChurchFeast = "Ευτροπίου, Κλεονίκου, Βασιλίσκου μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:54 Δύση Ήλιου 18:21"

            ElseIf DayOfYear = 63 Then
                ChurchFeast = "Γερασίμου του εν ΙΖορδάνη, Παύλου και Ιουλιανής μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:52 Δύση Ήλιου 18:22"

            ElseIf DayOfYear = 64 Then
                ChurchFeast = "Κόνωνος οσίου, Αρχελάου, Ευλαμπίου, "
                NameFeast = "γιορτάζουν οι: Αρχέλαος, Ευλόγιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:51 Δύση Ήλιου 18:23"

            ElseIf DayOfYear = 65 Then
                ChurchFeast = "Εύρ. Τιμίου Σταυρού, Εν Αμορίω 42 μαρτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:49 Δύση Ήλιου 18:24"

            ElseIf DayOfYear = 66 Then
                ChurchFeast = "Λαυρεντίου οσίου, Βασιλέως και Αιθερίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:48 Δύση Ήλιου 18:25"

            ElseIf DayOfYear = 67 Then
                ChurchFeast = "Θεοφυλάκτου Νικομ.-Ερμού, Δομετίου, "
                NameFeast = "γιορτάζει ο Ερμής. "
                WorldDay = "Διεθνής Ημέρα της Γυνέκας."
                SunRise = "Ανατολή Ήλιου 6:47 Δύση Ήλιου 18:26"

            ElseIf DayOfYear = 68 Then
                ChurchFeast = "+Των έν Σεβαστεία Αγ. 40 Μαρτύρων, Καισαρίου " + vbCrLf + "ιατρού (Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βιβιάνα, Ηλιανός, Ηλιάνα, " + vbCrLf + "Λυσίμαχος, Σμαράγδα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:45 Δύση Ήλιου 18:27"

            ElseIf DayOfYear = 69 Then
                '10 Μαρτίου
                ChurchFeast = "Κορδάτου Κορίνθου, Αναστασίας Πατρικίας, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:44 Δύση Ήλιου 18:28"

            ElseIf DayOfYear = 70 Then
                ChurchFeast = "Σωφρονίου Ιεροσαλ., Θαλλού, Θεοδώρας Άρτης, "
                NameFeast = "γιορτάζει ο Θαλλής. "
                WorldDay = "Ημέρα Των Θυμάτων Της Τρομοκρατίας."
                SunRise = "Ανατολή Ήλιου 6:42 Δύση Ήλιου 18:29"

            ElseIf DayOfYear = 71 Then
                ChurchFeast = "Συμεών Θεολόγου, Θεοφάνους ομολ., Γρηγορίου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:41 Δύση Ήλιου 18:30"

            ElseIf DayOfYear = 72 Then
                ChurchFeast = "Νικηφόρου Κωνστ/λεως, Πουπλίου Αθηνών, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:39 Δύση Ήλιου 18:31"

            ElseIf DayOfYear = 73 Then
                ChurchFeast = "Βενεδίκτου οσίου, Ευσχήμονος ομολογ. "
                NameFeast = "Γιορτάζουν οι: Βενέδικτος, Βενεδίκτη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:38 Δύση Ήλιου 18:32"

            ElseIf DayOfYear = 74 Then
                ChurchFeast = "Αγαπίου, Αριστοβούλου, Τιμολάου, "
                NameFeast = "γιορτάζει ο Αγάπιος. "
                WorldDay = "Παγκόσμια Ημέρα Καταναλωτή, Παγκόσμια Ημέρα" + vbCrLf + "Κατά της Αστυνομικής Βαρβαρότητας."
                SunRise = "Ανατολή Ήλιου 6:36 Δύση Ήλιου 18:33"

            ElseIf DayOfYear = 75 Then
                ChurchFeast = "Χριστοδούλου Πάτμου, Σαββίνου, "
                NameFeast = "γιορτάζουν οι: Ιουλιανός, Χριστόδουλος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:35 Δύση Ήλιου 18:33"

            ElseIf DayOfYear = 76 Then
                ChurchFeast = "+ Αλεξίου Ανθρώπου Θεού, Θεοστηρίκτου, "
                NameFeast = "γιορτάζουν οι: Αλέξιος, Αλεξία, Αλέξής, Αλέκος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:33 Δύση Ήλιου 18:34"

            ElseIf DayOfYear = 77 Then
                ChurchFeast = "Κυρίλλου, Τροφίμου, Ευκαρπίωνος, "
                NameFeast = ""
                WorldDay = " Διεθνής Ημέρα κατά της Κατοχής του Ιράκ."
                SunRise = "Ανατολή Ήλιου 6:32 Δύση Ήλιου 18:35"

            ElseIf DayOfYear = 78 Then
                ChurchFeast = "Χρυσάνθου και Δαρείας, Κλαυδίου μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Ιωσήφ, Δαρεία, Χρύσανθος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:30 Δύση Ήλιου 18:36"

            ElseIf DayOfYear = 79 Then
                '20 Μαρτίου
                ChurchFeast = "Εν μονή Αγ. Σάββα αναιρθέντων, Μύρωνος, "
                NameFeast = "γιορτάζει η Ροδή. "
                WorldDay = "Διεθνής Ημέρα της γής, Παγκόσμια Ημέρα Αποχής " + vbCrLf + "από το Κρέας, Διεθνής Ημέρα Αστρολογίας," + vbCrLf + "Παγκοσμια Ημέρα Θεάτρου για τα Παιδιά Και" + vbCrLf + "τους Νέους, Διεθνούς Ημέρα Γαλλοφωνίας."
                SunRise = "Ανατολή Ήλιου 6:28 Δύση Ήλιου 18:37"

            ElseIf DayOfYear = 80 Then
                ChurchFeast = "Μαρίας Αιγιπτίας, Ιακώβου ομολογητού, " + vbCrLf + "Φιλήμονος, Θωμά Κων/λεως, "
                NameFeast = "γιορτάζει ο Ιάκωβος. "
                WorldDay = "Παγκόσμια ημέρα κατα του ρατσισμού, Παγκόσμια " + vbCrLf + "ημέρα Δασοπονίας, Παγκόσμια ημέρα Ποίησης, " + vbCrLf + "Παγκόσμια ημέρα Ύπνου."
                SunRise = "Ανατολή Ήλιου 6:27 Δύση Ήλιου 18:38"

            ElseIf DayOfYear = 81 Then
                ChurchFeast = "Βασιλείου Αγκύρας, Καλλινίκης μάρτυρος," + vbCrLf + "Δροσίδος, "
                NameFeast = "γιορτάζουν οι: Δρόσος, Δροσούλα. "
                WorldDay = "Παγκόσμια ημέρα για το Νερό."
                SunRise = "Ανατολή Ήλιου 6:25 Δύση Ήλιου 18:39"

            ElseIf DayOfYear = 82 Then
                ChurchFeast = "Νίκωνος και των συν αυτό 199 μαρτύρον. "
                NameFeast = ""
                WorldDay = " Παγκόσμια ημέρα Μετεωρολογίας."
                SunRise = "Ανατολή Ήλιου 6:24 Δύση Ήλιου 18:40"

            ElseIf DayOfYear = 83 Then
                ChurchFeast = "Αρτέμονος ιερομαρτ., Ζαχαρίου, Παρθενίου, "
                NameFeast = ""
                WorldDay = " Παγκόσμια ημέρα κατά της Φυματίωσης."
                SunRise = "Ανατολή Ήλιου 6:22 Δύση Ήλιου 18:41"

            ElseIf DayOfYear = 84 Then
                ChurchFeast = "+ΕΥΑΓΓΕΛΙΣΜΟΣ ΤΗΣ ΘΕΟΤΟΚΟΥ (Κατάλυσης ιχθύος)" + vbCrLf + "(Εθν.Εορτή). "
                NameFeast = "γιορτάζουν οι: Ευάγγελος, Ευαγγελία, Βαγγέλης, " + vbCrLf + "Εθνεγερσία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:21 Δύση Ήλιου 18:42"

            ElseIf DayOfYear = 85 Then
                ChurchFeast = "+Σύναξις Αρχαγγέλου Γαβριήλ, Στεφάνου ομολ. " + vbCrLf + "(Κατάλυσης οίνου και ελαίου) "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:19 Δύση Ήλιου 18:43"

            ElseIf DayOfYear = 86 Then
                ChurchFeast = "Μαρτώνης, Φιλητού,Λυδίας, Παύλου Κορ. "
                NameFeast = "Γιορτάζουν οι: Αμφιλόχιος, Αμφιλοχία, Λυδία. "
                WorldDay = "Παγκόσμια ημέρα Θεάτρου."
                SunRise = "Ανατολή Ήλιου 6:18 Δύση Ήλιου 18:44"

            ElseIf DayOfYear = 87 Then
                ChurchFeast = "+Ιλαρίωνος οσίου, Ηρωδίωνος αποστ. εκ των 70, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:45"

            ElseIf DayOfYear = 88 Then
                ChurchFeast = "Μάρκου επ. Αρεθουσίων. Ευσταθίου, Κυρίλλου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:15 Δύση Ήλιου 18:46"

            ElseIf DayOfYear = 89 Then
                '30 Μαρτίου
                ChurchFeast = "Ευβούλης, Ζαχαρίου επισκόπου Κορίνθου. "
                NameFeast = ""
                WorldDay = "Παγκόσμια ημέρα Αντισύλληψης"
                SunRise = "Ανατολή Ήλιου 6:13 Δύση Ήλιου 18:47"

            ElseIf DayOfYear = 90 Then
                ' 31 Μαρτίου
                ChurchFeast = "Υπατίου επ. Γαγγρών, Ακακίου ιερομ. επ. Μελιτίνης. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:12 Δύση Ήλιου 18:47"

            ElseIf DayOfYear = 91 Then
                '1η Απριλίου
                ChurchFeast = "Μαρίας Αιγυπτίας, Γεροντίου μ., Μακαρίου οσ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:10 Δύση Ήλιου 18:48"

            ElseIf DayOfYear = 92 Then
                ChurchFeast = "Τίνου οσ., Θεοδώρας μαρτ., Αιδεσίου, Αμφιανού μ. "
                NameFeast = ""
                WorldDay = " Παγκόσμια ημέρα Παιδικού Βιβλίου"
                SunRise = "Ανατολή Ήλιου 6:09 Δύση Ήλιου 18:49"

            ElseIf DayOfYear = 93 Then
                ChurchFeast = "Νικήτα ομολογητού, Ιωσήφ Υμνογράφου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:07 Δύση Ήλιου 18:50"

            ElseIf DayOfYear = 94 Then
                ChurchFeast = "Γεωργίου εν Μαλεώ, Ζωσιμά, Πλάτωνος "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:06 Δύση Ήλιου 18:51"

            ElseIf DayOfYear = 95 Then
                ChurchFeast = "Κλαυδίου, Διοδώρου μαρτ., Θεοδώρας Θεσ/νίκης. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα των Προσφύγων."
                SunRise = "Ανατολή Ήλιου 6:04 Δύση Ήλιου 18:52"

            ElseIf DayOfYear = 96 Then
                ChurchFeast = "Ευτυχίου Πατρ. Κων/λεως, Πλατωνίδος οσίας, "
                NameFeast = "γιορτάζουν οι: Ευτύχης, Ευτυχία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:03 Δύση Ήλιου 18:53"

            ElseIf DayOfYear = 97 Then
                ChurchFeast = "Γεωργίου οσ., Καλλιοπίου και Ακυλίνης μαρτ. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Υγείας."
                SunRise = "Ανατολή Ήλιου 6:01 Δύση Ήλιου 18:53"

            ElseIf DayOfYear = 98 Then
                ChurchFeast = "Αγάβου, Ερμού, Ρούφου μαρτύρων, Ηρωδίωνος. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:00 Δύση Ήλιου 18:55"

            ElseIf DayOfYear = 99 Then
                ChurchFeast = "Ευψυχίου μαρτ., Βαδίμου, Γριγοριου Ε Κων/λεως, " + vbCrLf + "Τερεντίου, Δήμου νομ. "
                NameFeast = "Γιορτάζουν οι: Ευψύχιος, Ιωσήφ, Δημοσθένης, " + vbCrLf + "Ιωσηφίνα, Ραφαήλ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:58 Δύση Ήλιου 18:56"

            ElseIf DayOfYear = 100 Then
                '10 Απριλίου
                ChurchFeast = "Ευψυχίου μαρτυρος, Βαδίμου οσιομάρτυρος, "
                NameFeast = "γιορτάζουν οι: Δημοσθένης, Διονύσιος, Περικλής, " + vbCrLf + "Ηρακλής."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:57 Δύση Ήλιου 18:57"

            ElseIf DayOfYear = 101 Then
                ChurchFeast = "Αντίπα Περγάμου-Τρυφαίνης οσίας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά της ασθενείας Πάρκινσον."
                SunRise = "Ανατολή Ήλιου 5:55 Δύση Ήλιου 18:58"

            ElseIf DayOfYear = 102 Then
                ChurchFeast = "Ανθούσης, Παρίου ομολογ., Ακακίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:54 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 103 Then
                ChurchFeast = "Μαρτίνου Πάπα Ρώμης, θεοχάρους. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:51 Δύση Ήλιου 19:00"

            ElseIf DayOfYear = 104 Then
                ChurchFeast = "Αριστάρχου, Πούδη, Τροφίμου, θομαΐδος, "
                NameFeast = "γιορτάζουν οι: Αρίσταρχος, Θομαΐς, Λεωνίδας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:50 Δύση Ήλιου 19:01"

            ElseIf DayOfYear = 105 Then
                ChurchFeast = "Λεωνίδου επισκ. Αθηνών, Κρήσκεντος μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Αρίσταρχος, Θωμαΐς, Λεωνήδας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:48 Δύση Ήλιου 19:02"

            ElseIf DayOfYear = 106 Then
                ChurchFeast = "Αγάπης, Ειρήνης, Χιονίας Μάρτυρος. "
                NameFeast = "Σήμερα γιορτάζει η Γαλήνη. "
                WorldDay = "Παγκόσμια Ημέρα Φωνής."
                SunRise = "Ανατολή Ήλιου 5:47 Δύση Ήλιου 19:03"

            ElseIf DayOfYear = 107 Then
                ChurchFeast = "Μακαρίου Κορίνθου, Συμεών επ. Περσίδος. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Αγροτικής Πάλης"
                SunRise = "Ανατολή Ήλιου 5:45 Δύση Ήλιου 19:04"

            ElseIf DayOfYear = 108 Then
                ChurchFeast = "Ιωάννου οσίου-Κυριλλου Κωνστ/λεως. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Πολοτιστικής Κληρονομιάς."
                SunRise = "Ανατολή Ήλιου 5:44 Δύση Ήλιου 19:05"

            ElseIf DayOfYear = 109 Then
                ChurchFeast = "Παφνουτίου ιερομ.Φιλίππας μάρτυρος. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:44 Δύση Ήλιου 19:05"

            ElseIf DayOfYear = 110 Then
                '20 Απριλίου
                ChurchFeast = "Θεοδώρου τριχινά, Ζακχαίου, Αθανασίου. "
                NameFeast = ""
                WorldDay = " Διεθνής Ημέρα Ευαισθητοποίησης για το Θόρυβο."
                SunRise = "Ανατολή Ήλιου 5:43 Δύση Ήλιου 19:06"

            ElseIf DayOfYear = 111 Then
                ChurchFeast = "Ιανουαρίου ιερομ., Αναστασίου Σιναΐτου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:41 Δύση Ήλιου 19:07"

            ElseIf DayOfYear = 112 Then
                ChurchFeast = "Θεοδώρου οσ., Ναθαναήλ επισκ., Νεάρχου. "
                NameFeast = ""
                WorldDay = " Ημέρα της Γης."
                SunRise = "Ανατολή Ήλιου 5:40 Δύση Ήλιου 19:08"

            ElseIf DayOfYear = 113 Then
                ChurchFeast = "Λαζάρου Νεομάρτυρος"
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα βιβλίου."
                SunRise = "Ανατολή Ήλιου 5:39 Δύση Ήλιου 19:09"

            ElseIf DayOfYear = 114 Then
                ChurchFeast = "Ελισάβετ οσίας, Ξενοφώντος εν Άθω, "
                NameFeast = "γιορτάζουν οι: Αχιλλέας, Βάϊος, Βάϊα, Δάφνη," + vbCrLf + "Ελισάβετ. "
                WorldDay = "Παγκόσμια Ημέρα κατάργησης Πειραμάτων σε Ζώα"
                SunRise = "Ανατολή Ήλιου 5:37 Δύση Ήλιου 19:10"

            ElseIf DayOfYear = 115 Then
                ChurchFeast = "+Μάρκου αποστόλου, Νίκης Μάρτυρος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Μάρκος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:36 Δύση Ήλιου 19:11"

            ElseIf DayOfYear = 116 Then
                ChurchFeast = "Βασιλείου Αμασείας, Γλαφύρας, Ιούστας, "
                NameFeast = "γιορτάζει η Γλαφύρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:35 Δύση Ήλιου 19:11"

            ElseIf DayOfYear = 117 Then
                ChurchFeast = "Συμεών Ιεροσολύμων ιερομάρτυρας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Σχεδίου (Desing)."
                SunRise = "Ανατολή Ήλιου 5:34 Δύση Ήλιου 19:12"

            ElseIf DayOfYear = 118 Then
                ChurchFeast = "Των εν Κυζίκω 9 Μαρτύρων-Θεόγνιδος και" + vbCrLf + "των συν αυτώ. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα για την Υγεία και την " + vbCrLf + "Ασφάλεια στην Εργασία."
                SunRise = "Ανατολή Ήλιου 5:32 Δύση Ήλιου 19:13"

            ElseIf DayOfYear = 119 Then
                ChurchFeast = "Ιάσονος και Σωσιπάτρου, Ιωαννου Καλοκτένους, "
                NameFeast = "γιορτάζουν οι: Ιάσων, Κέρκυρα. "
                WorldDay = "Παγκόσμια Ημέρα Χορού."
                SunRise = "Ανατολή Ήλιου 5:31 Δύση Ήλιου 19:14"

            ElseIf DayOfYear = 120 Then
                ChurchFeast = "+Ιακώβου αποστόλου-Κλήμεντος οσίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ασημάκης, Ασημίνα, Δονάτος," + vbCrLf + "Ιάκωβος. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:30 Δύση Ήλιου 19:15"

            ElseIf DayOfYear = 121 Then
                '1η Μαΐου
                ChurchFeast = "Ιερεμίου προφ., Ευθυμίου, "
                NameFeast = "γιορτάζει ο Ιερεμίας. "
                WorldDay = "ΕΡΓΑΤΙΚΗ ΕΟΡΤΗ ΠΡΩΤΟΜΑΓΙΑ."
                SunRise = "Ανατολή Ήλιου 5:29 Δύση Ήλιου 19:16"

            ElseIf DayOfYear = 122 Then
                ChurchFeast = "+Ανακομιδή λειψάνον Μεγ. Αθανασίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Έσπερος, Εσπέρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:26 Δύση Ήλιου 19:17"

            ElseIf DayOfYear = 123 Then
                ChurchFeast = "Τιμοθέου και Μαύρας, Πέτρου Άργους, "
                NameFeast = "γιορτάζει η Ροδόπη "
                WorldDay = "Παγκόσμια Ημέρα Ελευθεροτυπίας Παγκόσμια " + vbCrLf + "ημέρα κατά του Άσθματος."
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:18"

            ElseIf DayOfYear = 124 Then
                ChurchFeast = "Πελαγίας, Ιλαρίου οσίου, Αντωνίου μαρτ. "
                NameFeast = "Γιορτάζουν οι: Πελαγία, Θεοχάρης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:25 Δύση Ήλιου 19:19"

            ElseIf DayOfYear = 125 Then
                ChurchFeast = "+Ειρήνης Μεγαλομ., Ευθυμίου Μαδύτου, "
                NameFeast = "γιορτάζουν οι: Ειρήνη, Ρένα, Ειρηναίος, " + vbCrLf + "Ειρηναία, Ευραίμ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:24 Δύση Ήλιου 19:20"

            ElseIf DayOfYear = 126 Then
                ChurchFeast = "Ιώβ του πολυάθλου, Σεραφιμ οσίου, "
                NameFeast = "γιορτάζουν οι: Ζήσης, Ζωή, Ζώης, Πηγή Σεραφείμ. "
                WorldDay = "Παγκόσμια Ημέρα κατά της Δίαιτας."
                SunRise = "Ανατολή Ήλιου 5:23 Δύση Ήλιου 19:21"

            ElseIf DayOfYear = 127 Then
                ChurchFeast = "Του εν Ουρανώ Φανέντος Τιμίου Σταυρού. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Γέλιου."
                SunRise = "Ανατολή Ήλιου 5:22 Δύση Ήλιου 19:22"

            ElseIf DayOfYear = 128 Then
                ChurchFeast = "+Ιωάννου του Θεολόγου και Ευαγγελιστού-" + vbCrLf + "Αρσενίου Μεγ. (Κατάλυσης οίνου και ελαίου) "
                NameFeast = "Γιορτάζει ο Θεολόγος. "
                WorldDay = "Παγκόσμια Ημέρα Ερυθρού Σταυρού και Ερυθράς " + vbCrLf + "Ημισελήνου. "
                SunRise = "Ανατολή Ήλιου 5:21 Δύση Ήλιου 19:23"

            ElseIf DayOfYear = 129 Then
                ChurchFeast = "+ Χριστοφόρου Μάρτυρος-Ησαΐου προφήτ. "
                NameFeast = "Γιορτάζουν οι: Ησαΐας, Χριστόφορος. "
                WorldDay = "Ημέρα της Ευρώπης."
                SunRise = "Ανατολή Ήλιου 5:20 Δύση Ήλιου 19:23"

            ElseIf DayOfYear = 130 Then
                '10 Μαΐου
                ChurchFeast = "Σίμωνος Ζηλωτού, Λαυρεντίου, Αλφειού, "
                NameFeast = "γιορτάζει ο Σίμος. "
                WorldDay = "Παγκόσμια Ημέρα της Μητέρας."
                SunRise = "Ανατολή Ήλιου 5:19 Δύση Ήλιου 19:24"

            ElseIf DayOfYear = 131 Then
                ChurchFeast = "Κυρίλου και Μεθοδίου, Μωκίου, Διοσκόρου, "
                NameFeast = "γιορτάζουν οι: Μεθόδιος, Ολυμπία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:18 Δύση Ήλιου 19:25"

            ElseIf DayOfYear = 132 Then
                ChurchFeast = "Επιφανείου Κύπρ., Γερμανού, Θεοδώρου. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Αδελφών Νοσοκόμων."
                SunRise = "Ανατολή Ήλιου 5:17 Δύση Ήλιου 19:26"

            ElseIf DayOfYear = 133 Then
                ChurchFeast = "Ευθυμίου Ιβήρων, Γλυκερίας, Σεργίου ομολογητού, "
                NameFeast = "γιορτάζει η Γλυκερία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:27"

            ElseIf DayOfYear = 134 Then
                ChurchFeast = "Θεράποντος, Ισιδώρου εν Χίω, Λεοντίου, Ιωάννου, "
                NameFeast = "γιορτάζουν οι: Ισίδωρος, Ισιδώρα, Αριστοτέλης, " + vbCrLf + "Τέλης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:28"

            ElseIf DayOfYear = 135 Then
                ChurchFeast = "+ Παχωμίου Μεγαλομ., Αχιλλίου Λαρίσης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Αχίλιος, Κάλη, Καλη. "
                WorldDay = "Διεθνής Ημέρα Οικογένειας."
                SunRise = "Ανατολή Ήλιου 5:15 Δύση Ήλιου 19:29"

            ElseIf DayOfYear = 136 Then
                ChurchFeast = "Θεοδώρου Ηγιασμένου, Γεωργίου Μυτιλ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:14 Δύση Ήλιου 19:30"

            ElseIf DayOfYear = 137 Then
                ChurchFeast = "Ανδρονίκου και Ιουνίας απ. εκ των 70, " + vbCrLf + "Αθανασίου Χριστιαν, "
                NameFeast = "γιορτάζουν οι: Ανδρόνικος, Ανδρονίκη, Ιουνία. "
                WorldDay = "Παγκόσμια Ημέρα Τηλεπικοινωνιων."
                SunRise = "Ανατολή Ήλιου 5:13 Δύση Ήλιου 19:30"

            ElseIf DayOfYear = 138 Then
                ChurchFeast = "Πέτρου-Διονυσίου, Ανδρέου, Ιουλίας μάρτ. "
                NameFeast = "Γιορτάζουν οι: Γαλάτεια, Ιουλία. "
                WorldDay = "Παγκόσμια Ημέρα Μουσείων."
                SunRise = "Ανατολή Ήλιου 5:12 Δύση Ήλιου 19:31"

            ElseIf DayOfYear = 139 Then
                ChurchFeast = "Πατρικίου Προύσσης, Μενάνδρου μάρτ. "
                NameFeast = "Γιορτάζουν οι: Μαγδαληνή, Μάγδα, Πατρίκιος. "
                WorldDay = "Ημέρα μνήμης της γενοκτονίας του Ποντιακού Ελληνισμού."
                SunRise = "Ανατολή Ήλιου 5:12 Δύση Ήλιου 19:32"

            ElseIf DayOfYear = 140 Then
                '20 Μαΐου
                ChurchFeast = "+ Αν. Λειψ. Αγ. Νικολάου, Νικήτα Χίου, " + vbCrLf + "Θαλλελαίου μαρτ. "
                NameFeast = "Γιορτάζει η Λυδία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:11 Δύση Ήλιου 19:33"

            ElseIf DayOfYear = 141 Then
                ChurchFeast = "+Κωνσταντίνου και Ελένής, Βισαρίωνος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι:Ελένη, Ελεάννα, Κωωσταντίνος, " + vbCrLf + "Κωνσταντίνα. "
                WorldDay = "Παγκόσμια Ημέρα Πολιτισμου."
                SunRise = "Ανατολή Ήλιου 5:10 Δύση Ήλιου 19:34"

            ElseIf DayOfYear = 142 Then
                ChurchFeast = "Πάυλου και Δημητρίου Τριπόλ., Βασιλίσκου, "
                NameFeast = "γιορτάζουν οι: Αιμίλιος, Αιμιλία, Εμυ, Εμιλία," + vbCrLf + "Επιφάνειος. "
                WorldDay = "Παγκόσμια Ημέρα Βιοποικιλότητας."
                SunRise = "Ανατολή Ήλιου 5:09 Δύση Ήλιου 19:35"

            ElseIf DayOfYear = 143 Then
                ChurchFeast = "Μιχαήλ Συνάδων, Συνεσίου Επισκόπου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:09 Δύση Ήλιου 19:35"

            ElseIf DayOfYear = 144 Then
                ChurchFeast = "Συμεών οσίου, Μαρκιανής, Σωσάννης, "
                NameFeast = "γιορτάζει ο Μέλέτης. "
                WorldDay = "Ευρωπαϊκή Ημέρα Πάρκων."
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 145 Then
                ChurchFeast = "+Γ' Εύρεσις Κεφαλής Τιμίου Προδρόμου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα αλληλεγγύης στους λαούς, που" + vbCrLf + "αγωνίζονται για την Ελευθερία Ανεξαρτισία και τα" + vbCrLf + "Ανθρώπινα Δικαιώματα, Παγκόσμια Ημέρα Αφρικής."
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 146 Then
                ChurchFeast = "Κάρπου και Αλφαίου εκ των 70 Απ., Αλεξάνδρου νεομ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:37"

            ElseIf DayOfYear = 147 Then
                ChurchFeast = "+ Ιωάννου Ρώσου, Ελλαδίου ιερ.,Θεράποντος ιερομ. "
                NameFeast = "Γιορτάζουν οι: Αλύπιος, Αλυπία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:07 Δύση Ήλιου 19:38"

            ElseIf DayOfYear = 148 Then
                ChurchFeast = "Ευτυχούς επ. Μελιτινής, Ελικωνίδος, Νικήτα " + vbCrLf + "Χαλκηδόνας. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:39"

            ElseIf DayOfYear = 149 Then
                ChurchFeast = "Θεοδοσίας Παρθενομάρτυρος, Υπομονής οσίας, "
                NameFeast = "Σήμερα γιορτάζει η Θεοδοσία. "
                WorldDay = "Παγκόσμια Ημέρα Κυανοκράνων."
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:40"

            ElseIf DayOfYear = 150 Then
                '30 Μαΐου
                ChurchFeast = "Ισακίου ηγουμ. Μονής Δαλμάτων, Εμμελείας μητρός " + vbCrLf + "Μ. Βασιλείου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:41"

            ElseIf DayOfYear = 151 Then
                '31 Μάΐου
                ChurchFeast = "Ερμείου Αποστόλου και μάρτυρος, ευσεβίου μ., " + vbCrLf + "Ευσταθίου Κων/λεως. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Κατά του Καπνίσματος."
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:41"

            ElseIf DayOfYear = 152 Then
                '1η Ιουνίου
                ChurchFeast = "Ιουστίνου Μάρτυρος, Πύρρου οσίου, "
                NameFeast = "γιορτάζουν οι: Γεράκης, Γερακία, Ευέλπιστος, " + vbCrLf + "Ιουστίνος, Πύρρος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:42"

            ElseIf DayOfYear = 153 Then
                ChurchFeast = "Νικηφόρου Κωνστ., Εράσμου, Κωνσταντήνου. "
                NameFeast = "γιορτάζουν οι: Νικηφόρος, Μαρίνος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:43"

            ElseIf DayOfYear = 154 Then
                ChurchFeast = "Λουκιλλιανόυ και Πάύλης και των συν αυτοίς. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:43"

            ElseIf DayOfYear = 155 Then
                ChurchFeast = "Μάρθας και Μαρίας αδ. Λαζάρου, Μητροφάνους " + vbCrLf + "Κωνσ/λεως, "
                NameFeast = "γιορτάζει η Μάρθα. "
                WorldDay = "Διεθνής Ημέρα κατά της επιθετικότητας εναντίον των " + vbCrLf + "Παιδιών."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:44"

            ElseIf DayOfYear = 156 Then
                ChurchFeast = "Δωροθέου επ. Τύρου, Νικανδρου, Γοργίου. "
                NameFeast = "γιορτάζουν οι: Απόλλων, Δωρόθέα. "
                WorldDay = "Παγκόσμια Ημέρα Περιβάλοντος."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 157 Then
                ChurchFeast = "Θεοδώρου Ηγιασμένου, Ιλαρίονος Δαλμάτ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 158 Then
                ChurchFeast = "Θεοδότου Αγκ., Ζηναϊδος, Σεβαστιανής. "
                NameFeast = "γιορτάζει η Σεβαστιανή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:46"

            ElseIf DayOfYear = 159 Then
                ChurchFeast = "+Ανακομιδή λειψάνον Θεοδώρου Στρατηλάτου, Καλλιόπης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει η Καλλιόπη. "
                WorldDay = "Παγκόσμια Ημέρα των Ωκεανών."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:46"

            ElseIf DayOfYear = 160 Then
                ChurchFeast = "Κυρίλου αρχιεπισκόπου Αλεξανδρείας, Ανανίου μαρτ.," + vbCrLf + "Θεοφάνους Εγκ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 161 Then
                '10 Ιουνίου
                ChurchFeast = "Αλεξάνδρου-Αντονίνης μάρτυρος Τιμοθέου Προύσης, "
                NameFeast = "γιορτάζει η Σάρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 162 Then
                ChurchFeast = "+Βαρθολομαίου και Βαρνάβα των Αποστόλων, Λουκά " + vbCrLf + "Ρώσου Ιατρού (Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βαρθολομαίος, Βαρνάβας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 163 Then
                ChurchFeast = "Ονουφρίου οσίου Πέτρου εν Άθω, "
                NameFeast = "γιορτάζει ο Ονούφριος. "
                WorldDay = "Παγκόσμια Ημέρα κατά της Παιδικής Εργασίας."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 164 Then
                ChurchFeast = "Ακυλίνης, Αντιπάτρου, τριφυλλίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 165 Then
                ChurchFeast = "Ελισσαίου προφήτου, Μεθοδίου, κυρίλλου, "
                NameFeast = "γιορτάζει ο Ελισσαίος. "
                WorldDay = "Παγκόσμια Ημέρα του εθελοντού Αιμοδότη, Διεθνής" + vbCrLf + "Ημέρα των Webloggers."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 166 Then
                ChurchFeast = "Αμώς προφ., Ιερωνυμου, Στεφανά, Ιωνά, "
                NameFeast = "γιορτάζουν οι: Αυγουστίνα, Αυγουστίνος, Αύγουστος," + vbCrLf + "Μόνικα. "
                WorldDay = "Παγκόσμια Ημέρα του Πατέρα Παγκόσμια Ημέρα" + vbCrLf + "Γονιμότητας."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 167 Then
                ChurchFeast = "Τύχωνος, Μάρκου, Απολλων Ιάδος. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 168 Then
                ChurchFeast = "Ισαύρου, Μανουήλ, Σαβέλ, Ισμαήλ μαρτύρων. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά της Ερημοποίησης και της " + vbCrLf + "Ξηρασίας."
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 169 Then
                ChurchFeast = "Λεοντίου, Υπατίου Θεοδούλου Αιθερίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 170 Then
                ChurchFeast = "Ιούδα Θαδδαίου Θεαδέλφου, Ζήνωνος, "
                NameFeast = "γιορτάζει ο Παΐσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 171 Then
                '20 Ιουνίου
                ChurchFeast = "Μεθοδίου Πατάρων, Νικολάου Καβάσιλα, "
                NameFeast = "γιορτάζουν οι: Κορίνα, Κόρη. "
                WorldDay = "Παγκόσμια Ημέρα Προσφύγων."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 172 Then
                ChurchFeast = "Ιουλιανού, Νικήτα, Τερεντίου, Αφροδισίου. "
                NameFeast = "γιορτάζουν οι: Αφροδίσιος, Αφροδισία. "
                WorldDay = "Ευρωπαϊκή Ημέρα Μουσικής, Παγκόσμια Ημέρα " + vbCrLf + "Υδρογραφίας."
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 173 Then
                ChurchFeast = "Ευσεβίου, Ζήνονος, Ζηνά, Ιουλιανής, "
                NameFeast = "γιορτάζουν οι: Ευσέβιος, Σέβη, Ευσεβούλα, Ζήνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 174 Then
                ChurchFeast = "Αγριππίνης μάρτυρος, Αριστοκλέους, "
                NameFeast = "γιορτάζουν οι: Αγριππίνα, Αριστοκλής, Λουλού, Λούλης. "
                WorldDay = "Ολυμπιακή Ημέρα"
                SunRise = "Ανατολή Ήλιου 5:03 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 175 Then
                ChurchFeast = "+ Γέννεση Ι.Προδρόμου, Παναγιώτου νεομάρτυρος " + vbCrLf + "(Κατάλυσης ιχθύος). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:02 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 176 Then
                ChurchFeast = "Φεβρωνίας, ευφροσύνης, Μεθοδίου, "
                NameFeast = "γιορτάζουν οι: Έρως, Έρωτας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 177 Then
                ChurchFeast = "Δαβιδ Θεσαλονίκης, Ιωάνου Γοτθίας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά των ναρκωτικών, Διεθνής Ημέρα" + vbCrLf + "κατά των Βασανισμών."
                SunRise = "Ανατολή Ήλιου 5:04 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 178 Then
                ChurchFeast = "Σαμψών Ξενοδόχου, Ιοάννας Μυροφόρου. "
                NameFeast = ""
                WorldDay = " Ημέρα Ομοφυλόφιλης Υπερηφάνειας."
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 179 Then
                ChurchFeast = "Ανακ. Λειψάνον Κύρου και Ιωάννου Αναργύρων, "
                NameFeast = "γιορτάζει ο Γερμανός"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:05 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 180 Then
                ChurchFeast = "+ ΠΕΤΡΟΥ ΚΑΙ ΠΑΥΛΟΥ Των αποστόλων " + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν  οι: Πέτρος, Παύλος, Παυλίνα," + vbCrLf + "Πετρούλα. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 181 Then
                '30 Ιουνίου
                ChurchFeast = "+ Η Σύναξις των Δώδεκα Αποστόλων " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Απόστολος, Αποστολία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 182 Then
                '1η Ιουλίου
                ChurchFeast = "+Κοσμα και Δαμιανού των αναργύρων" + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ανάργυρος, Ανάργυρη, Δαμιανός," + vbCrLf + "Κοσμάς."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:06 Δύση Ήλιου 19:52"

            ElseIf DayOfYear = 183 Then
                ChurchFeast = "+Καταθ. Τιμίας Εσθήτος Θεοτ, Ιουβεναλίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:07 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 184 Then
                ChurchFeast = "Υακίνθου, Ανατολίου, Γερασίμου Καρπεν, "
                NameFeast = "γιορτάζουν οι: Ζουμπουλία, Υάκινθος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:07 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 185 Then
                ChurchFeast = "Ανδρέου Κρήτης, Λουκίας, Θεοδότου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 186 Then
                ChurchFeast = "Αθανασίου εν Άθω, Λαμπαδού, Σεργίου. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Συνεταιρισμών."
                SunRise = "Ανατολή Ήλιου 5:08 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 187 Then
                ChurchFeast = "Σισώη, Αρχίππου, Επιμάχου, Στεφάνου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:09 Δύση Ήλιου 19:51"

            ElseIf DayOfYear = 188 Then
                ChurchFeast = "+ Κυριακής Μεγαλομ., Λουκιανού, Θωμά, "
                NameFeast = "γιορτάζουν οι: Κυριακή, Κική, Κίκα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:10 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 189 Then
                ChurchFeast = "+ Προκοπίου Μεγαλομ., Θεοδοσίας μάρτ. "
                NameFeast = "Γιορτάζουν οι: Θεόφιλος, Προκόπης. "
                WorldDay = "Παγκόσμια Ημέρα Αλλεργίας."
                SunRise = "Ανατολή Ήλιου 5:10 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 190 Then
                ChurchFeast = "Παγκρατίου ιερομ, Πρόβου, Μεθοδίου, "
                NameFeast = "γιορτάζει η Βερόνικα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:11 Δύση Ήλιου 19:50"

            ElseIf DayOfYear = 191 Then
                '10 Ιουλίου
                ChurchFeast = "Εν Νικοπόλει 45 Μαρτ. Απολλωνίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:11 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 192 Then
                ChurchFeast = "+ Ευφημίας Μεγαλομ., Όλγας, Νεκταρίου, "
                NameFeast = " γιορτάζει οι: Ευφημία, Όλγα. "
                WorldDay = "Παγκόσμια Ημέρα Πληθυσμού"
                SunRise = "Ανατολή Ήλιου 5:12 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 193 Then
                ChurchFeast = "Πρόκλου, Ιλαρίου, Βερονίκης, Σάββα. "
                NameFeast = "γιορτάζουν οι: Βερενίκη, Βερόνικα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:13 Δύση Ήλιου 19:49"

            ElseIf DayOfYear = 194 Then
                ChurchFeast = "Σύν.Αρχ. Γαβριήλ, Στεφάνου Σαβαΐτου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:13 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 195 Then
                ChurchFeast = "+ Νικοδήμου οσ., Ιωσήφ Θεσ/νίκης, Ακύλα. "
                NameFeast = "Σήμερα γιοτράζουν οι: Ακύλας, Νικόδημος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:14 Δύση Ήλιου 19:48"

            ElseIf DayOfYear = 196 Then
                ChurchFeast = "Κηρύκου και Ιουλίττης, Ματρώνης Χίου, "
                NameFeast = "γιορτάζουν οι: Βλαδίμηρος, Ιουλίττα, Κήρυκος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:15 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 197 Then
                ChurchFeast = "Αθηνογένους, Αντιόχου μαρτ., Φαύστου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:47"

            ElseIf DayOfYear = 198 Then
                ChurchFeast = "+ Μαρίνης μεγαλομ., Βερονίκης, Σπεραίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Μάρίνα, Μαρίνος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:16 Δύση Ήλιου 19:46"

            ElseIf DayOfYear = 199 Then
                ChurchFeast = "Αιμιλιανού, Υακίνθου, Ουαλεντίνης μαρτ. "
                NameFeast = "Γιορτάζουν οι: Αιμιλιανός, Αιμίλιος, Αιμιλιανή," + vbCrLf + "Αιμιλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:17 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 200 Then
                ChurchFeast = "Μακρίνης και Δίου, Ευγενίου, Στεφάνου, "
                NameFeast = "γιορτάζουν οι: Διός, Δίας"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:18 Δύση Ήλιου 19:45"

            ElseIf DayOfYear = 201 Then
                ' 20 Ιουλίου
                ChurchFeast = "+ Προφήτου Ηλίου Θεσβίτου, Φλαβιανού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ηλίας"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:19 Δύση Ήλιου 19:44"

            ElseIf DayOfYear = 202 Then
                ChurchFeast = "Ιωάννου, Συμεόν του Σαλού, Ονουφρίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:19 Δύση Ήλιου 19:43"

            ElseIf DayOfYear = 203 Then
                ChurchFeast = "+Μαρίας Μαγδαλην. Μυροφ., Μαρκέλλης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορταζουν οι: Μαρκέλα, Μαγδαληνή, Μάγδα. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:21 Δύση Ήλιου 19:42"

            ElseIf DayOfYear = 204 Then
                ChurchFeast = "Φωκά ιερομ., Ιεζεκιήλ, Βιταλίου, Σωσάννης. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:22 Δύση Ήλιου 19:41"

            ElseIf DayOfYear = 205 Then
                ChurchFeast = "Χριστίνης, Αφηναγόρου, Υμεναίου μάρτυρως, "
                NameFeast = "γιορτάζει ο Αθηναγόρας"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:22 Δύση Ήλιου 19:40"

            ElseIf DayOfYear = 206 Then
                ChurchFeast = "+Κοίμησις Αγίας Άννης, Ευπραξίας " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Άννα, Ανούλα, Αννίτα, Αννέτα," + vbCrLf + "Ευπραξία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:23 Δύση Ήλιου 19:40"

            ElseIf DayOfYear = 207 Then
                ChurchFeast = "+ Παρασκευής οσιομάρτυρος, Ερμολάου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει η Παρασκευή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:24 Δύση Ήλιου 19:39"

            ElseIf DayOfYear = 208 Then
                ChurchFeast = "+Παντελεήμονος Ιαματικού, Ανθούσης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = " γιορτάζει ο Παντελής."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:25 Δύση Ήλιου 19:38"

            ElseIf DayOfYear = 209 Then
                ChurchFeast = "Προχόρου, Νικάνορος, Ερήνης Χρυσοβ, "
                NameFeast = "γιορτάζουν οι Ακάκιος, Αυξέντιος, Αυξεντία, " + vbCrLf + "Δρόσος, Δροσούλα. "
                WorldDay = "Διεθνής Ημέρα του Διαχειριστή Συστημάτων"
                SunRise = "Ανατολή Ήλιου 5:26 Δύση Ήλιου 19:37"

            ElseIf DayOfYear = 210 Then
                ChurchFeast = "+ Καλλινίκου και Θεοδότης, Βασιλίσκου, "
                NameFeast = "γιορτάζει ο Καλλίνικος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 211 Then
                '30 Ιουλίου
                ChurchFeast = "Σίλα, Ανδρονίκου Επαινετού, Σιλουανού, "
                NameFeast = "γιορτάζουν οι: Ανδρόνικος, Ανδρονίκη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:36"

            ElseIf DayOfYear = 212 Then
                ChurchFeast = "Προεόρτια Προόδου Τιμίου Σταυρού,"
                NameFeast = " γιορτάζουν οι: Ευδόκιμος, Ιωσήφ, Σήφις, " + vbCrLf + "Ιωσηφίνα, Ζοζεφίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:27 Δύση Ήλιου 19:35"

            ElseIf DayOfYear = 213 Then
                '1η Αυγούστου
                ChurchFeast = "+Πρόοδος Τιμίου Σταυρου (αρχή νηστείας)., 7 " + vbCrLf + "Παίδων, Σολομονής. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:28 Δύση Ήλιου 19:34"

            ElseIf DayOfYear = 214 Then
                ChurchFeast = "Ανακομ. λειψάνου Στεφάνου, Θεοδωρου νεομ."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:29 Δύση Ήλιου 19:33"

            ElseIf DayOfYear = 215 Then
                ChurchFeast = "Δαλμάτου, Φαύστρου, Ισαακίου, Σαλώμης, "
                NameFeast = "γιορτάζει η Σαλώμη. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:30 Δύση Ήλιου 19:32"

            ElseIf DayOfYear = 216 Then
                ChurchFeast = "Των εν Εφέσω 7 Παίδων, Ευδοκίας. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:31 Δύση Ήλιου 19:31"

            ElseIf DayOfYear = 217 Then
                ChurchFeast = "Ευσιγνίου, Ευθυμίου, Νόνας, Φαβίου. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:32 Δύση Ήλιου 19:31"

            ElseIf DayOfYear = 218 Then
                ChurchFeast = "+ Η ΜΕΤΑΜΟΡΦΩΣΙΣ ΤΟΥ ΣΩΤΗΡΟΣ " + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν  οι: Σωτήριος, Σωτηρία, Ευμορφία, " + vbCrLf + "Πορφούλα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:32 Δύση Ήλιου 19:30"

            ElseIf DayOfYear = 219 Then
                ChurchFeast = "Δομετίου, Θεοδοσίου, Αργολίδος, "
                NameFeast = "γιορτάζουν οι: Αστέριος, Αστέρης, Αστέρω, Αστερία,  " + vbCrLf + "Αστρινή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:33 Δύση Ήλιου 19:29"

            ElseIf DayOfYear = 220 Then
                ChurchFeast = "Αιμιλιανού, Μύρωνος, τριανταφύλλου, "
                NameFeast = "γιορτάζουν οι Τριαντάφυλλος, Τριανταφυλλιά."
                WorldDay = "Παγκόσμια Ημέρα Οργασμού"
                SunRise = "Ανατολή Ήλιου 5:34 Δύση Ήλιου 19:28"

            ElseIf DayOfYear = 221 Then
                ChurchFeast = "Ματθία αποστ., Ψόη, Μαρίας Πατρκίας. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα των Αυτοχθόνων Λαών του Κόσμου."
                SunRise = "Ανατολή Ήλιου 5:35 Δύση Ήλιου 19:25"

            ElseIf DayOfYear = 222 Then
                '10 Αυγούστου
                ChurchFeast = "Λαυρεντίου αρχιδιακ., Ξύστου Ιππολύτου, "
                NameFeast = "γορτάζουν οι: Ευλαμπία, Ηρώ, Ιππόλυτος, Λαυρέντης, " + vbCrLf + "Λαυρεντία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:36 Δύση Ήλιου 19:24"

            ElseIf DayOfYear = 223 Then
                ChurchFeast = "Εύπλου, Θάυμα Αγ. Σπυρίδων., Νεοφύτου"
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:37 Δύση Ήλιου 19:23"

            ElseIf DayOfYear = 224 Then
                ChurchFeast = "Ανικήτου, Παμφίλου, Φωτίου, Καπίτωνος. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Νεολαίας"
                SunRise = "Ανατολή Ήλιου 5:38 Δύση Ήλιου 19:22"

            ElseIf DayOfYear = 225 Then
                ChurchFeast = "Μαξίμου ομ., Ευδοκίας, Τύχωνος, Ξένης"
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Αριστερόχειρων"
                SunRise = "Ανατολή Ήλιου 5:39 Δύση Ήλιου 19:21"

            ElseIf DayOfYear = 226 Then
                ChurchFeast = "Προεόρτια Κοιμ.Θεοτ., Μιχαίου, Συμεών"
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:40 Δύση Ήλιου 19:19"

            ElseIf DayOfYear = 227 Then
                ChurchFeast = "+ Η ΚΟΙΜΗΣΙ ΤΗΣ ΘΕΟΤΟΚΟΥ (Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν οι: Μαρία, Μαριάνθη, Παναγιώτα, " + vbCrLf + "Παναγιώτης, Μάριος, Δέσποινα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:40 Δύση Ήλιου 19:18"

            ElseIf DayOfYear = 228 Then
                ChurchFeast = "Διομήδους, Αλκιβιάδου, Αγ. Μανδηλιου, "
                NameFeast = "γιορτάζουν οι: Αλκιβιάδης, Αλκίνοος, Γεράσιμος, " + vbCrLf + "Σταμάτης"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:41 Δύση Ήλιου 19:17"

            ElseIf DayOfYear = 229 Then
                ChurchFeast = "Μύρωνος, Κυπριανού, Στράτωνος μάρτ., "
                NameFeast = "γιορτάζει η Λευκοθέα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:42 Δύση Ήλιου 19:15"

            ElseIf DayOfYear = 230 Then
                ChurchFeast = "Φλώρου, Λαύρου, Ερμού Λέοντος μάρτ., "
                NameFeast = "γιορτάζουν οι: Αρσένιος, Αρσένης, Αρσενία, " + vbCrLf + "Φλώρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:43 Δύση Ήλιου 19:14"

            ElseIf DayOfYear = 231 Then
                ChurchFeast = "Ανδρέου Στρατηλ., Θέκλης, Θεοφάνους. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:44 Δύση Ήλιου 19:13"

            ElseIf DayOfYear = 232 Then
                '20 Αυγούστου
                ChurchFeast = "Σαμουήλ προφ., Ηλιοδωρου, Μέμνονος, "
                NameFeast = "γιορτάζει ο Ηλιόδωρος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:45 Δύση Ήλιου 19:11"

            ElseIf DayOfYear = 233 Then
                ChurchFeast = "Θαδδαίου αποστ., Βάσσης, Θεοκλητούς. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:46 Δύση Ήλιου 19:10"

            ElseIf DayOfYear = 234 Then
                ChurchFeast = "Αγαθονίκου, Ακινδύνου, Ανθούσης μάρτ., "
                NameFeast = "γιορτάζει ο Αγαθώνικος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:47 Δύση Ήλιου 19:09"

            ElseIf DayOfYear = 235 Then
                ChurchFeast = "+ Αποδ.Κοιμ. Θεοτ., Παναγίας Προύσσης"
                NameFeast = ""
                WorldDay = "Διεθνής Ημέρα κατά του δουλεμπορίου"
                SunRise = "Ανατολή Ήλιου 5:47 Δύση Ήλιου 19:07"

            ElseIf DayOfYear = 236 Then
                ChurchFeast = "+Κοσμά Αιτωλού, Ανακ. Λειψάν. Διονυσίου, "
                NameFeast = "γιορτάζουν οι: Αιτωλία, Ευτύχης, Ευτυχία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:48 Δύση Ήλιου 19:06"

            ElseIf DayOfYear = 237 Then
                ChurchFeast = "Βαρθολομαίου και Τίτου Αποστ., ευμενίου, "
                NameFeast = "γιορτάζει ο Βαρθολομαίος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:49 Δύση Ήλιου 19:04"

            ElseIf DayOfYear = 238 Then
                ChurchFeast = "Αδριανου και Ναταλίας, Ιωασάφ, Αττικού, "
                NameFeast = "γιορτάζουν οι: Αντριάνα, Ανδριανός, Ναταλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:50 Δύση Ήλιου 19:00"

            ElseIf DayOfYear = 239 Then
                ChurchFeast = "+ Φανουρίου Μεγαλομ., Ποιμένος, Ανθούσης, "
                NameFeast = "γιορτάζουν οι: Φανούρης, Φάνης, Φανή, Αρκαδία, " + vbCrLf + "Αρκάδα, Αρκάδιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:51 Δύση Ήλιου 19:00"

            ElseIf DayOfYear = 240 Then
                ChurchFeast = "Μωϋσέως Αιθίοπ., Διομήδους, Λαυρεντίου, "
                NameFeast = "γιορτάζουν οι: Δάμων, Ευκίνη. "
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:52 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 241 Then
                ChurchFeast = "+Αποτομή Κεφ. Ιοάννου και βαπτιστού, Θεοπίστης " + vbCrLf + "οσ.(νηστεία). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:53 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 242 Then
                ' 30 Αυγούστου
                ChurchFeast = "Αλεξάνδρου, Ιωάνου και Πάύλου Πατριαρχ.Κων/λεως, "
                NameFeast = "γιορτάζουν οι: Αλέξανδρος, Αλέκος, Ευλάλιος, " + vbCrLf + "Ζηνοβία."
                WorldDay = "Διεθνής Ημέρα Εξαφανισμένων"
                SunRise = "Ανατολή Ήλιου 5:53 Δύση Ήλιου 18:57"

            ElseIf DayOfYear = 243 Then
                ChurchFeast = "+Κατάθεσις Τιμίας Ζώνης Υπεραγίας, Θεοτόκου," + vbCrLf + "Αριστείδου (Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:54 Δύση Ήλιου 18:56"

            ElseIf DayOfYear = 244 Then
                '1η Σεμτεμβρίου
                ChurchFeast = "+Αρχή Ινδίκτου, Μελετίου οσ., Συμεών Στυλίτου," + vbCrLf + "Μελέτιου (Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Συμεών, Αθηνά, Αντιγώνη, Ερατώ," + vbCrLf + "Θάλεια, Κλειώ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:55 Δύση Ήλιου 18:54"

            ElseIf DayOfYear = 245 Then
                ChurchFeast = "Μαμάντος, Φιλαδέλφου, Ιωάνν. νηστευτού, "
                NameFeast = "Μαμάντος, Φιλαδέλφου, Ιωάνν. νηστευτού."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:56 Δύση Ήλιου 18:53"

            ElseIf DayOfYear = 246 Then
                ChurchFeast = "Ανθίμου ιερομ., Θεοκτίστου, Πολυδώρου, "
                NameFeast = "γιορτάζουν οι: Ανθιμος, Αρίστη, Αριστίων, Αρχοντή," + vbCrLf + "Αρχοντία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:57 Δύση Ήλιου 18:51"

            ElseIf DayOfYear = 247 Then
                ChurchFeast = "Βαβύλα, Μωϋσέως πρ., Ερμιόνης, Βεβαίας, "
                NameFeast = "γιορτάζουν οι: Ερμιόνη, Μοϋσής, Ροζαλία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:58 Δύση Ήλιου 18:50"

            ElseIf DayOfYear = 248 Then
                ChurchFeast = "Ζαχαρίου, Αβδαίου, Ουβρανού, Θεοδώρου, "
                NameFeast = "γιορτάζει ο Ζαχαρίας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:59 Δύση Ήλιου 18:48"

            ElseIf DayOfYear = 249 Then
                ChurchFeast = "+Ανάμνησις θαύματος Αρχιστρατήγου Μιχαήλ " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βίβος, Βιβή, Ευδόξιος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 5:59 Δύση Ήλιου 18:47"

            ElseIf DayOfYear = 250 Then
                ChurchFeast = "Ευόδου και Ονησιφόρου, Σώζοντος μάρτυρος, "
                NameFeast = "γιορτάζει η Κασσιανή."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:00 Δύση Ήλιου 18:45"

            ElseIf DayOfYear = 251 Then
                ChurchFeast = "+ ΓΕΝΝΕΣΙΟΝ ΘΕΟΤΟΚΟΥ, Ρούφου μάρτυρος " + vbCrLf + "(Κατάλυσης ιχθύος). "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα κατά του Αναλφαβητισμού."
                SunRise = "Ανατολή Ήλιου 6:01 Δύση Ήλιου 18:44"

            ElseIf DayOfYear = 252 Then
                ChurchFeast = "+ΠΡΟ ΥΨΩΣΕΩΣ Σύναξις Θεοπατόρων Ιωακείμ και" + vbCrLf + "Άννης(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ιωακείμ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:02 Δύση Ήλιου 18:42"

            ElseIf DayOfYear = 253 Then
                ' 10 Σεπτεμβρίου
                ChurchFeast = "Μηνοδώρας, Μητροδώρας, Νυμφοδώρας, "
                NameFeast = "γιορτάζουν οι: Εράστη, Έραστος, Κλημεντίνη, " + vbCrLf + "Κλήμης, Μητροδώρα."
                WorldDay = " Παγκόσμια Ημέρα κατά της Αυτοκτονίας."
                SunRise = "Ανατολή Ήλιου 6:03 Δύση Ήλιου 18:41"

            ElseIf DayOfYear = 254 Then
                ChurchFeast = "Θεοδώρας, Πάυλου Εφέσ., Ευανθίας μαρτ., "
                NameFeast = "γιορτάζει η Ευανθία"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:04 Δύση Ήλιου 18:39"

            ElseIf DayOfYear = 255 Then
                ChurchFeast = "Αυτονόμου, μάρτ., Δανιήλ του Θασίου, Κουρνούτου, "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:04 Δύση Ήλιου 18:37"

            ElseIf DayOfYear = 256 Then
                ChurchFeast = "+Κορνηλίου Ιεροθέου, Ιβηρίτου Κρονίδου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Αριστείδης, Αριστέας, Άρης, Αριστέα," + vbCrLf + "Κορνήλιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:05 Δύση Ήλιου 18:36"

            ElseIf DayOfYear = 257 Then
                ChurchFeast = "+ΥΨΩΣΗΣ ΤΟΥ ΤΙΜΙΟΥ ΣΤΑΥΡΟΥ (Νηστεία), "
                NameFeast = "γιορτάζουν οι: Θεοκλής, Σταύρος, Σταυρούλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:06 Δύση Ήλιου 18:34"

            ElseIf DayOfYear = 258 Then
                ChurchFeast = "Νικήτα, Βησσαρίωνος, Νικολάου Κρήτης, "
                NameFeast = "γιορτάζουν οι: Βησσαρίων, Νικήτας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:07 Δύση Ήλιου 18:33"

            ElseIf DayOfYear = 259 Then
                ChurchFeast = "+Ευφημίας μεγαλομάρτυρος, Μελιτίνης μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Ευφημία, Λουντμίλλα. "
                WorldDay = "Παγκόσμια Ημέρα για τη διατήρηση της στοιβάδας του " + vbCrLf + "όζοντος."
                SunRise = "Ανατολή Ήλιου 6:08 Δύση Ήλιου 18:31"

            ElseIf DayOfYear = 260 Then
                ChurchFeast = "+ Σοφίας, Πίστεως, Αγάπης, Ελπίδος μαρτ., "
                NameFeast = "γιορτάζουν οι: Σοφία, Πίστη, Ελπίδα, Αγάπη, Αγαθοκλής," + vbCrLf + "Σόνια"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:09 Δύση Ήλιου 18:30"

            ElseIf DayOfYear = 261 Then
                ChurchFeast = "Ευμενίου, Κάστορος, Αριάδνης μάρτυρος, "
                NameFeast = "γιορτάζουν οι Ευμένιος, Αριάδνη, Ρωμύλος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:10 Δύση Ήλιου 18:28"

            ElseIf DayOfYear = 262 Then
                ChurchFeast = "Τροφίμου, Σαββατίου, Δορυμέδοντος μαρ., "
                NameFeast = "γιορτάζει ο Σαββάτιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:10 Δύση Ήλιου 18:27"

            ElseIf DayOfYear = 263 Then
                '20 Σεπτεμβρίου
                ChurchFeast = "+Ευσταθίου μεγαλ., Θεοπίστης, Μαρτίνου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου)"
                NameFeast = "γιορτάζουν οι: Στάθης, Ευσταθία"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:11 Δύση Ήλιου 18:25"

            ElseIf DayOfYear = 264 Then
                ChurchFeast = "+ΜΕΤΑ ΤΗΝ ΥΨΩΣΙΝ, Κορδάτου, Ιωνά, Μελετίου, " + vbCrLf + "Πρίσκου μαρτ., "
                NameFeast = ""
                WorldDay = " Διεθνής Ημέρα Ειρήνης, Παγκόσμια Ημέρα Αλτσχάιμερ."
                SunRise = "Ανατολή Ήλιου 6:12 Δύση Ήλιου 18:23"

            ElseIf DayOfYear = 265 Then
                ChurchFeast = "Φωκά ιερομ., Ισαάκ και Μαρτίνου μαρτύρων, "
                NameFeast = "γιορτάζει η Ζωγραφιά. "
                WorldDay = "Ευρωπαϊκή Ημέρα Χωρίς Αυτοκίνητο."
                SunRise = "Ανατολή Ήλιου 6:13 Δύση Ήλιου 18:22"

            ElseIf DayOfYear = 266 Then
                ChurchFeast = "+Σύλληψις Ιωάννου Προδρόμου, Ξανθίππης " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ξανθίππη, Ξανθή, Πολυξένη, " + vbCrLf + "Ξένη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:14 Δύση Ήλιου 18:20"

            ElseIf DayOfYear = 267 Then
                ChurchFeast = "+ Ανάμνησις Θαύματος Μυρτιδιωτίσσης, "
                NameFeast = " γιορτάζουν οι: Θέκλα, Μυρσίνη, Περσεφόνη"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:15 Δύση Ήλιου 18:19"

            ElseIf DayOfYear = 268 Then
                ChurchFeast = "Ευφοσύνης Παφνουτίου, Τάττης μάρτ. "
                NameFeast = "γιορτάζουν οι: Ευφροσύνη, Φρόσύνη, Φρόσω"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:17"

            ElseIf DayOfYear = 269 Then
                ChurchFeast = "+Μετάστασις Ιωάννου Θεολόγου, Γεδεών " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = ""
                WorldDay = " Ευρωπαϊκή Ημέρα Γλωσσών"
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:16"

            ElseIf DayOfYear = 270 Then
                ChurchFeast = "Καλλιστράτου, Αριστάρχου, Γαϊανής, "
                NameFeast = "γιορτάζουν οι Ακυλίνα, Ακυλίνη, Ζήνων. "
                WorldDay = "Παγκόσμια Ημέρα Τουρισμού."
                SunRise = "Ανατολή Ήλιου 6:17 Δύση Ήλιου 18:14"

            ElseIf DayOfYear = 271 Then
                ChurchFeast = "Χαρίτωνος ομολ., Νεοφύτου, Βαρούχ προφ. "
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:16 Δύση Ήλιου 18:12"

            ElseIf DayOfYear = 272 Then
                ChurchFeast = "Κυριακού αναχωρ., Δορυμέδοντος, Δάδα., "
                NameFeast = "γιορτάζει ο Κυριάκος. "
                WorldDay = "Παγκόσμια Ναυτική Ημέρα."
                SunRise = "Ανατολή Ήλιου 6:19 Δύση Ήλιου 18:11"

            ElseIf DayOfYear = 273 Then
                '30 Σεπτεμβρίου
                ChurchFeast = "Γρηγορίου, Στρατονίκου, Μαδρονίου μάρτ. "
                NameFeast = ""
                WorldDay = " Παγκόσμια Ημέρα Καρδιάς."
                SunRise = "Ανατολή Ήλιου 6:20 Δύση Ήλιου 18:09"

            ElseIf DayOfYear = 274 Then
                ' 1η Οκτωβρίου
                ChurchFeast = "Ανανίου αποστόλου, Ρωμανού Μελωδού, "
                NameFeast = "γιορτάζουν οι: Θηρεσία, Ρομανός. "
                WorldDay = "Παγκόσμια Ημέρα Κατοικίας, Παγκόσμια Ημέρα Τρίτης " + vbCrLf + "Ηλικίας, Πάγκόσμια Ημέρα κατά της Ηπατίτιδας C."
                SunRise = "Ανατολή Ήλιου 6:21 Δύση Ήλιου 18:08"

            ElseIf DayOfYear = 275 Then
                ChurchFeast = "Κυπριανου ιερομ., Ιουστίνης μάρτυρος, "
                NameFeast = "γιορτάζουν οι: Ιουστίνη, Κυπριανός."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:22 Δύση Ήλιου 18:06"

            ElseIf DayOfYear = 276 Then
                ChurchFeast = "+Διονυσίου Αεροπαγίτου, Δαμάριδος, "
                NameFeast = "γιορτάζει ο Διονύσιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:23 Δύση Ήλιου 18:05"

            ElseIf DayOfYear = 277 Then
                ChurchFeast = "Ιεροθέου επισκ. Αθηνών, Δομνίνης μαρτ., "
                NameFeast = "γιορτάζουν οι: Βρίνα, Βρίνη, Βέρα, Ιερόθεος," + vbCrLf + "Ιεροθέα. "
                WorldDay = "Παγκόσμια Ημέρα των Ζώων."
                SunRise = "Ανατολή Ήλιου 6:24 Δύση Ήλιου 18:03"

            ElseIf DayOfYear = 278 Then
                ChurchFeast = "Χαριτίνη, Ευδοκίμου, Μεθοδιας εν Κιμώλω, "
                NameFeast = "γιορτάζει η Χαριτίνη. "
                WorldDay = "Παγκόσμια Ημέρα Εκπαιδευτικών."
                SunRise = "Ανατολή Ήλιου 6:25 Δύση Ήλιου 18:02"

            ElseIf DayOfYear = 279 Then
                ChurchFeast = "+Θωμά αποστόλου, Ερωτηΐδος μάρτυρος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου)."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:25 Δύση Ήλιου 18:00"

            ElseIf DayOfYear = 280 Then
                ''''''''''''''
                ChurchFeast = "Σεργίου και Βάκχου μαρτ., Πολυχρονίου, "
                NameFeast = "γιορτάζουν οι: Βάκχος, Πολυχρόνης, Χρόνης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:26 Δύση Ήλιου 18:59"

            ElseIf DayOfYear = 281 Then
                ChurchFeast = "Πελαγίας οσίας, Πελαγίας παρθένου, " + vbCrLf + "Ταϊσίας οσίας, "
                NameFeast = "γιορτάζει η Πελαγία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:27 Δύση Ήλιου 17:57"

            ElseIf DayOfYear = 282 Then
                ChurchFeast = "+ Ιακώβου του Αλφαίου αποστ., Ανδρονίκου " + vbCrLf + "και Αθανασίας, "
                NameFeast = "γιορτάζει ο Αβραάμ. "
                WorldDay = "Παγκόσμια Ημέρα Ταχυδρομείου"
                SunRise = "Ανατολή Ήλιου 6:28 Δύση Ήλιου 17:56"

            ElseIf DayOfYear = 283 Then
                ' 10 Οκτωβρίου
                ChurchFeast = "Ευλαμπίου και Ευλαμπίας μαρτ.,Θεοφίλου οσίου, "
                NameFeast = "γιορτάζει ο Ευλάμπιος. "
                WorldDay = "Διεθνής Ημέρα μείωσης των φυσικών Καταστροφών, " + vbCrLf + "Παγκόσμια Ημερα ψυχικής Υγείας," + vbCrLf + "Παγκόσμια Ημέρα κατά της Θανατικής ποινής."
                SunRise = "Ανατολή Ήλιου 6:29 Δύση Ήλιου 17:54"

            ElseIf DayOfYear = 284 Then
                ChurchFeast = "Φιλίππου διακόνου,Θεοφάνους Γραπτού"
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Όρασης (Κατά της Τύφλωσης)."
                SunRise = "Ανατολή Ήλιου 6:30 Δύση Ήλιου 17:53"

            ElseIf DayOfYear = 285 Then
                ChurchFeast = "Πρόβου, Ταράχου, Ανδρονίκου, Συμεών νέου Θεολόγου, "
                NameFeast = "γιορτάζουν οι: Ανδρομάχη, Μάχη, Ανδρόμαχος, Βαλάντιος. "
                WorldDay = "Παγκόσμια Ημέρα Αυγού, Παγκόσμια Ημέρα κατά της " + vbCrLf + "Αρθρίτιδας."
                SunRise = "Ανατολή Ήλιου 6:31 Δύση Ήλιου 17:52"

            ElseIf DayOfYear = 286 Then
                ChurchFeast = "Κάρπου, Παπύλου, Αγαθοδώρου μαρτύρων, "
                NameFeast = "γιορτάζει η Αγαθονίκη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:32 Δύση Ήλιου 17:50"

            ElseIf DayOfYear = 287 Then
                ChurchFeast = "Ναζαρίου, Γερβασίου, Κοσμά Μελωδού, "
                NameFeast = "γιορτάζει ο Γερβάσιος. "
                WorldDay = "Παγκόσμια Ημέρα Προτύπων Τυποποίησης."
                SunRise = "Ανατολή Ήλιου 6:33 Δύση Ήλιου 17:49"

            ElseIf DayOfYear = 288 Then
                ChurchFeast = "Λουκιανού, Σαβίνου, Βάρσου, Ευθυμίου νεομάρτυρος, "
                NameFeast = "γιορτάζει ο Λουκιανός. "
                WorldDay = "Διεθνής Ημέρα του Λευκού Μπαστουνιού."
                SunRise = "Ανατολή Ήλιου 6:34 Δύση Ήλιου 17:47"

            ElseIf DayOfYear = 289 Then
                ChurchFeast = "Λογγίνου εκατοντάρχου, Λεοντίου και των συν αυτώ. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Διατροφής, Παγκόσμια Ημέρα" + vbCrLf + "Σπονδυλικής Στήλης."
                SunRise = "Ανατολή Ήλιου 6:35 Δύση Ήλιου 17:46"

            ElseIf DayOfYear = 290 Then
                ChurchFeast = "Ωσηέ προφήτου, Ανδρέου του εν κρίσει, "
                NameFeast = "γιορτάζουν οι: Αντίγονος, Ευπρέπιος. "
                WorldDay = "Παγκόσμια Ημέρα Για την εξάλειψη της Φτώχειας, " + vbCrLf + "Παγκόσμια Ημέρα Μυοσκελετικού τραύματος."
                SunRise = "Ανατολή Ήλιου 6:36 Δύση Ήλιου 17:45"

            ElseIf DayOfYear = 291 Then
                ChurchFeast = "+Λουκά Ευαγγελιστού, Μαρίνου μάρτυρος " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Λουκάς Λουκία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:37 Δύση Ήλιου 17:43"

            ElseIf DayOfYear = 292 Then
                ChurchFeast = "Ιωήλ προφήτου, Ουάρου μαρτ., Κλεοπάτρας."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:38 Δύση Ήλιου 17:42"

            ElseIf DayOfYear = 293 Then
                '20 Οκτωβρίου
                ChurchFeast = "+Γερασίμου Κεφαλληνίας, Αρτεμίου, Μαρτώνης, "
                NameFeast = "γιορτάζουν οι:Αρτέμιος, Αρτέμης, Άρτεμις,Γεράσιμος," + vbCrLf + "Ενόη. "
                WorldDay = "Παγκόσμια Ημέρα Οστεοπόρωσης."
                SunRise = "Ανατολή Ήλιου 6:39 Δύση Ήλιου 17:40"

            ElseIf DayOfYear = 294 Then
                ChurchFeast = "Χριστοδ.εν Πάτμω, Ιλαρίωνος μεγαλομ., Σωκράτους"
                NameFeast = "γιορτάζουν οι: Ευκράτης, Ούρσουλα, Σωκράτης," + vbCrLf + "Χριστόδουλος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:40 Δύση Ήλιου 17:39"

            ElseIf DayOfYear = 295 Then
                ChurchFeast = "Αβερκιου Ιεραπόλεως, Γλυκερίας, Γαϊου, "
                NameFeast = "γιορτάζουν οι: Αβέρκιος, Αβερκία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:41 Δύση Ήλιου 17:38"

            ElseIf DayOfYear = 296 Then
                ChurchFeast = "+Ιακώβου απ.Αδελφοθέου, Ιγνατίου Κων/λεως " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ιάκωβος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:42 Δύση Ήλιου 17:37"

            ElseIf DayOfYear = 297 Then
                ChurchFeast = "Αρέθα μεγαλομ., Σεβαστιανής μάρτυρος, "
                NameFeast = "γιορτάζει η Σεβαστιανή. "
                WorldDay = "Ημέρα του ΟΗΕ."
                SunRise = "Ανατολή Ήλιου 6:43 Δύση Ήλιου 17:35"

            ElseIf DayOfYear = 298 Then
                ChurchFeast = "Ταβιθάς εν Ιοππη, Μαρκιανού, Μαρτυρίου, "
                NameFeast = "γιορτάζει η Χρυσάνθη."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:44 Δύση Ήλιου 17:34"

            ElseIf DayOfYear = 299 Then
                ChurchFeast = "+Δημητρίου Μεγαλομάρτυρος Μυροβλήτου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Δημήτριος, Δήμητρα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:45 Δύση Ήλιου 17:33"

            ElseIf DayOfYear = 300 Then
                ChurchFeast = "Νέστορος μεγαλομ., Πρόκλης συζ. Πιλάτου, "
                NameFeast = "γιορτάζει ο Νέστορας."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:46 Δύση Ήλιου 17:32"

            ElseIf DayOfYear = 301 Then
                ChurchFeast = "+Αγίας Σκέπης (Επέτειος του ΟΧΙ)."
                NameFeast = ""
                WorldDay = " (Εθνική εορτή)"
                SunRise = "Ανατολή Ήλιου 6:47 Δύση Ήλιου 17:30"

            ElseIf DayOfYear = 302 Then
                ChurchFeast = "Αναστασίας Ρωμαίας, Αβραμίου Μελιτινής. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Ψωρίασης."
                SunRise = "Ανατολή Ήλιου 6:48 Δύση Ήλιου 17:29"

            ElseIf DayOfYear = 303 Then
                '30 Οκτωβρίου
                ChurchFeast = "Κλεόπα, Αρτεμά, Ζηνοβίου και Ζηνοβίας, "
                NameFeast = "γιορτάζουν οι: Ζηνώβιος, Ζηνωβια, Απολλωνία, " + vbCrLf + "Αστέριος, Αστέρης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:49 Δύση Ήλιου 17:28"

            ElseIf DayOfYear = 304 Then
                ChurchFeast = "Στάχυος, Απελλού, Αμπλία, Αριστοβούλου, "
                NameFeast = "γιορτάζουν οι: Απελλής, Απέλλης, Αριστόβουλος, " + vbCrLf + "Αριστοβούλη."
                WorldDay = "Παγκόσμια Ημέρα Αποταμίευσης."
                SunRise = "Ανατολή Ήλιου 6:50 Δύση Ήλιου 17:27"

            ElseIf DayOfYear = 305 Then
                '1η Νοεμβρίου
                ChurchFeast = "+Αγίων Αναργύρων κοσμα και Δαμιανού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάουν οι: Ανάργυρος, Ανάργυρη, Κόσμας, " + vbCrLf + "Δαμιανός. "
                WorldDay = "Παγκόσμια Ημέρα Χορτοφαγίας."
                SunRise = "Ανατολή Ήλιου 6:51 Δύση Ήλιου 17:26"

            ElseIf DayOfYear = 306 Then
                ChurchFeast = "Ακινδύνου, Πηγασίου, Ελπιδοφόρου μάρτυρως, "
                NameFeast = "γιορτάζουν οι: Αφθόνιος, Αφθονία, Ελπιδοφόρος, Πήγασος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:52 Δύση Ήλιου 17:25"

            ElseIf DayOfYear = 307 Then
                ChurchFeast = "Ακεψιμά, Αειθαλά, Ιωσήφ Μαρτύρων."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:53 Δύση Ήλιου 17:24"

            ElseIf DayOfYear = 308 Then
                ChurchFeast = "Ιωαννικίου οσίου, Νικάνδρου, Ερμαίου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:54 Δύση Ήλιου 17:23"

            ElseIf DayOfYear = 309 Then
                ChurchFeast = "Γαλακτίωνος, Επιστήμης μαρτυρος."
                NameFeast = "γιορτάζουν οι: Γαλακτίων, Λίνος, Λίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:55 Δύση Ήλιου 17:22"

            ElseIf DayOfYear = 310 Then
                ChurchFeast = "Πάυλου κων/λεως, λουκα οσίου, Δημητριανού Κύπρου, "
                NameFeast = "γιορτάζει ο Λεονάρδος."
                WorldDay = "Παγκόσμια Ημέρα κατά της εκμετάλλευσης του " + vbCrLf + "περιβάλλοντος στον πόλεμο και της Ένοπλες Συγκρούσης."
                SunRise = "Ανατολή Ήλιου 6:56 Δύση Ήλιου 17:21"

            ElseIf DayOfYear = 311 Then
                ChurchFeast = "Των εν Μελιτινή 33 μαρτ., Λαζάρου οσίου, "
                NameFeast = "γιορτάζουν οι: Αθηνόδωρος, Θεαγένης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:57 Δύση Ήλιου 17:20"

            ElseIf DayOfYear = 312 Then
                ChurchFeast = "+Παμεγγίστων Ταξιαρχών Μιχαήλ και Γαβριήλ " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Μιχάλης, Άγγελος, Αγγελικη, " + vbCrLf + "Ταξιάρχης, Γαβριήλ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 6:58 Δύση Ήλιου 17:19"

            ElseIf DayOfYear = 313 Then
                ChurchFeast = "+Ονησιφόρου μαρτ., Νεκταρίου εν Αιγίνη, "
                NameFeast = "γιορτάζουν οι: Νεκτάριος, Νεκταρία, Ελλάδιος. "
                WorldDay = "Διεθνής Ημέρα κατά του Φασισμύ και του " + vbCrLf + "Αντισημιτισμου."
                SunRise = "Ανατολή Ήλιου 7:00 Δύση Ήλιου 17:18"

            ElseIf DayOfYear = 314 Then
                ' 10 Νοεμβρίου
                ChurchFeast = "Ολυμπά, Σωσιπάτρου, Ροδίωνος, Ορέστου, "
                NameFeast = "γιορτάζουν οι: Αρσένιος, Αρσένης, Αρσενία, " + vbCrLf + "Αρσιόνη, Ωρίων."
                WorldDay = "Παγκόσμια Ημέρα Επιστήμης για την Ειρήνη και " + vbCrLf + "την Ανάπτυξη."
                SunRise = "Ανατολή Ήλιου 7:01 Δύση Ήλιου 17:17"

            ElseIf DayOfYear = 315 Then
                ChurchFeast = "+Μηνά μεγαλομ., Βίκτωρος και Βικεντίου, "
                NameFeast = "γιορτάζουν οι: Μηνάς, Βικέντιος, Βίκτωρ, " + vbCrLf + "Βικτόρια, Δράκων."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:02 Δύση Ήλιου 17:16"

            ElseIf DayOfYear = 316 Then
                ChurchFeast = "+Ιωάννου Ελεήμονος, Νείλου οσίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου)."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:03 Δύση Ήλιου 17:15"

            ElseIf DayOfYear = 317 Then
                ChurchFeast = "+Ιωάννου Χρυσοστόμου, Δαμασκινού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Χρυσόστομος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:04 Δύση Ήλιου 17:15"

            ElseIf DayOfYear = 318 Then
                ChurchFeast = "+Φιλίππου αποστόλου, Κων/νου Υδραίου, " + vbCrLf + "Γρ.Παλαμά (Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζει ο Φίλιππος. "
                WorldDay = "Παγκόσμια Ημέρα κατά του Διαβήτη."
                SunRise = "Ανατολή Ήλιου 7:05 Δύση Ήλιου 17:14"

            ElseIf DayOfYear = 319 Then
                ChurchFeast = "Γουρία, Σαμμωνά, Αβίβου μαρτ." + vbCrLf + "(αρχή νηστίας.) "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Φιλοσοφίας."
                SunRise = "Ανατολή Ήλιου 7:06 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 320 Then
                ChurchFeast = "+Ματθαίου αποστόλου και Ευαγγελιστού" + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ματθαίος, Ιφιγένια, Ματθίλντη. "
                WorldDay = "Διεθνής Ημέρα Ανεκτικότητας."
                SunRise = "Ανατολή Ήλιου 7:07 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 321 Then
                ChurchFeast = "Γριγορίου Νεοκαισαρείας, Γενναδίου Κωνστ., "
                NameFeast = "γιορτάζει ο Γεννάδιος. "
                WorldDay = "Διεθνής Ημέρα Σπουδαστών."
                SunRise = "Ανατολή Ήλιου 7:08 Δύση Ήλιου 17:12"

            ElseIf DayOfYear = 322 Then
                ChurchFeast = "Πλάτωνος-Ρωμανού, Ζακχαίου μαρτύρων, "
                NameFeast = "γιορτάζει ο Πλάτων."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:09 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 323 Then
                ChurchFeast = "Αβδιού προφήτου, Βαρλαάμ μάρτυρος. "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Δημόσιας Τουαλέτας."
                SunRise = "Ανατολή Ήλιου 7:10 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 324 Then
                ' '20 Νοεμβρίου
                ChurchFeast = "Γριγορ. Δεκαπολίτου, Πρόκλου Κωνστ/λεως, "
                NameFeast = "γιορτάζουν οι: Δεναχίς, Δεναχίδα. "
                WorldDay = "Ημέρα για την Εκβιομηχάνιση της Αφρικής, " + vbCrLf + "Παγκόσμια Ημέρα για τα δικαιώματα του Παιδιού."
                SunRise = "Ανατολή Ήλιου 7:11 Δύση Ήλιου 17:10"

            ElseIf DayOfYear = 325 Then
                ChurchFeast = "+ΤΑ ΕΙΣΟΔΙΑ ΤΗΣ ΥΠΕΡΑΓΙΑΣ ΘΕΟΤΟΚΟΥ " + vbCrLf + "(Κατάλυσης ιχθύος), "
                NameFeast = "γιορτάζουν οι: Μαρία - για ανύπανρες, " + vbCrLf + "Μάριος, Σουλτάνα. "
                WorldDay = "Παγκόσμια Ημέρα Χρόνιας Αποφρακτικής " + vbCrLf + "Πνευμονοπάθειας, Παγκόσμια Ημέρα Τηλεόρασης," + vbCrLf + "Παγκόσμια Ημέρα Χαιρετισμού."
                SunRise = "Ανατολή Ήλιου 7:12 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 326 Then
                ChurchFeast = "φιλήμονος αποστόλου και των συν αυτώ, "
                NameFeast = "γιορτάζουν οι: Βαλεριανός, Βαλεριάνα, Βαλερία, Βαλέριος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:13 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 327 Then
                ChurchFeast = "Αμφιλοχίου επισκοπ.Ικονίου, Σισινίου ομολογ., "
                NameFeast = "γιορτάζουν οι: Μερόπη, Έλενος, Λένος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:14 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 328 Then
                ChurchFeast = "Κλήμεντος, Πέτρου Αλεξ/νδρ., Ερμογένους, "
                NameFeast = "γιορτάζει η Φλώρα. "
                WorldDay = "Παγκόσμια Ημέρα Αγοραστικής Αποχής."
                SunRise = "Ανατολή Ήλιου 7:15 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 329 Then
                ChurchFeast = "+Αικατερίνης μεγαλομ., Μερκουρίου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι : Κατερίνα, Αικατερίνη, Κατίνα," + vbCrLf + "Κυπαρισσία, Σίσσυ. "
                WorldDay = " Διεθνής Ημέρα κατά της κακοποίησης της " + vbCrLf + "Γυναίκας."
                SunRise = "Ανατολή Ήλιου 7:16 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 330 Then
                ChurchFeast = "+Στυλιανού Παφλαγ., Νίκωνος (Μετανοείτε), "
                NameFeast = "γιορτάζουν οι: Στέλιος Στέλλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:17 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 331 Then
                ChurchFeast = "Ιακώβου Πέρσου, Ναθαναήλ οσίου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:18 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 332 Then
                ChurchFeast = "Στεφάνου ομολογ., Ειρηνάρχου μάρτύρων, "
                NameFeast = "γιορτάζει ο Ειρήναρχος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:19 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 333 Then
                ChurchFeast = "Παραμόνου, Φαίδρου και 360 μαρτύρων, "
                NameFeast = "γιορτάζει η Φαίδρα. "
                WorldDay = "Διεθνής Ημέρα αλληλεγγύης προς τον " + vbCrLf + "Παλαιστινιακό Λαό."
                SunRise = "Ανατολή Ήλιου 7:20 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 334 Then
                '30 Νοεμβρίου
                ChurchFeast = "+Ανδρέου πρωτοκλήτου, Φρουμεντίου Ινδίας" + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = " γιορτάζουν οι: Ανδρέας, Ανδρίκος"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:21 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 335 Then
                '1η Δεκεβμρίου
                ChurchFeast = "Ναούμ προφήτου, Θεοκλήτου, Φιλαρέτου, "
                NameFeast = "γιορτάζουν οι: Ναούμ, Φιλάρετος, Ιακώβ. "
                WorldDay = "Παγκόσμια Ημέρα κατά του AIDS."
                SunRise = "Ανατολή Ήλιου 7:22 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 336 Then
                ChurchFeast = "Αββακούμ προφ., Μυρόπης, Θεοφίλου, "
                NameFeast = "γιορτάζουν οι: Μερόπη, Σολομών. "
                WorldDay = "Παγκόσμια Ημέρα για την εξάλειψη της Δουλείας."
                SunRise = "Ανατολή Ήλιου 7:23 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 337 Then
                ChurchFeast = "Σοφονίου προφ., Θεοδούλα οσ.,Αγγελή, "
                NameFeast = "γιορτάζει ο Γλυκέριος. "
                WorldDay = "Παγκόσμια Ημέρα Ατόμων με Ειδικές Ανάγκες."
                SunRise = "Ανατολή Ήλιου 7:24 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 338 Then
                ChurchFeast = "+Βαρβάρας μεγαλ., Ιωάννου Δαμασκηνού " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Βαρβάρα, Δαμασκηνός, Σεραφείμ."
                WorldDay = "Παγκόσμια Ημέρα κατά των Ναρκων."
                SunRise = "Ανατολή Ήλιου 7:25 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 339 Then
                ChurchFeast = "+Σάββα Ηγιασμένου, Διογένους, Νίνου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Διογένης, Σάββας. "
                WorldDay = "Παγκόσμια Ημέρα Εθελοντισμού για την Οικονομική" + vbCrLf + "και Κοινωνική Ανάπτυξη."
                SunRise = "Ανατολή Ήλιου 7:26 Δύση Ήλιου 17:05"

            ElseIf DayOfYear = 340 Then
                ChurchFeast = "+Νικολάου Μύρων, Νικολάου νεομάρτυρ " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Νίκος, Νικολέτα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:27 Δύση Ήλιου 17:05"

            ElseIf DayOfYear = 341 Then
                ChurchFeast = "Αμβροσίου Μεδιαλάνων, Νεοφύτου μαρτ., "
                NameFeast = "γιορτάζουν  οι: Αμβρόσιος, Αμβρόσης, Αμβροσία. "
                WorldDay = "Διεθνής Ημέρα Πολιτικής Αεροπορίας."
                SunRise = "Ανατολή Ήλιου 7:28 Δύση Ήλιου 17:05"

            ElseIf DayOfYear = 342 Then
                ChurchFeast = "+Παταπίου οσίου, Σωφρονίου, Σωσθένους, "
                NameFeast = "γιορτάζει ο Πατάπιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:29 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 343 Then
                ChurchFeast = "+Σύλληψις. Αγ. Άννης μητρός της Θεοτόκου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου). "
                NameFeast = "Γιορτάζουν οι: Άννα, Αννούλα, Αννίτα, Αννέτα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:30 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 344 Then
                '10 Δεκεμβρίου
                ChurchFeast = "Μηνά, Ερμογένους, Ευγράφου, Θεοτέκν."
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Ανθρωπίνων Δικαιωμάτων, " + vbCrLf + "Παγκόσμια Ημέρα Ιδιοκτησίας"
                SunRise = "Ανατολή Ήλιου 7:30 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 345 Then
                ChurchFeast = "Δανιήλ Στυλίτου, Αιμιλιανού, Βεβαίας μαρ., "
                NameFeast = "γιορτάζουν οι: Αρών, Αδάμ, Αδαμάντιος, " + vbCrLf + "Αδαμαντία, Δανάη. "
                WorldDay = "Παγκόσμια Ημέρα Βουνού."
                SunRise = "Ανατολή Ήλιου 7:31 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 346 Then
                ChurchFeast = "+Σπυρίδωνος Τριμυθούντος του θαυμ., Αλεξάνδρου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Σπύρος."
                WorldDay = "Διεθνής Ημέρα Παιδικής Τηλεόρασης."
                SunRise = "Ανατολή Ήλιου 7:32 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 347 Then
                ChurchFeast = "Ευγενίου, Λουκίας, Ευστρατίου, Αυξεντ., "
                NameFeast = "γιορτάζουν οι: Άρης, Στρατής, Στρατούλα, Λουκία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:06"

            ElseIf DayOfYear = 348 Then
                ChurchFeast = "Θύρσου, Απολλωνίου, Καλλινίκ., Λευκίου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 349 Then
                ChurchFeast = "+Ελευθερίου ιερομάρτ., Ανθίας, Βάκχου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Ελευθέριος, Ελευθερία, Ανθή, " + vbCrLf + "Ανθούλα, Σύλβια."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:33 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 350 Then
                ChurchFeast = "Αγγαίου, Μοδέστρου, Θεοφάνους, Πρόβου."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 351 Then
                ChurchFeast = "+Διονυσίου Αιγίνης, Δανιήλ προφ., Ιάκχου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζουν οι: Δανιήλ, Διονύσιος, Ιακχος, " + vbCrLf + "Ρεβέκκα, Ολυμπία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:35 Δύση Ήλιου 17:07"

            ElseIf DayOfYear = 352 Then
                ChurchFeast = "Νικοστράτου, Σεβαστιανού, Κάστορος, "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα Μετανάστη."
                SunRise = "Ανατολή Ήλιου 7:36 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 353 Then
                ChurchFeast = "Βονιφατίου, Ευτυχίου, Αγλαϊδος, Άρεως"
                NameFeast = "γιορτάζει η Αγλαΐα"
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:36 Δύση Ήλιου 17:08"

            ElseIf DayOfYear = 354 Then
                '20 Δεκεμβρίου
                ChurchFeast = "+Ιγνατίου Θεοφόρου, φιλογονίου, Ιωάννου " + vbCrLf + "(Κατάλυσης οίνου και ελαίου), "
                NameFeast = "γιορτάζει ο Ιγνάτιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:37 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 355 Then
                ChurchFeast = "Ιουλιανής, Θεμιστοκλέους, Πέτρου, "
                NameFeast = "γιορτάζουν οι: Θεμιστοκλης, Θέμης."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:09"

            ElseIf DayOfYear = 356 Then
                ChurchFeast = "Αναστασίας φαρμακ., Χρυσογόνου, "
                NameFeast = "γιορτάζει η Αναστασία."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:10"

            ElseIf DayOfYear = 357 Then
                ChurchFeast = "+ΠΡΟ ΧΡΙΣΤΟΥ ΓΕΝΝ., Εν. κρήτη 10 μαρτ."
                NameFeast = ""
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:38 Δύση Ήλιου 17:10"

            ElseIf DayOfYear = 358 Then
                ChurchFeast = "Παραμ. Χριστουγέννων, Ευγενίας, Υακίνθου, "
                NameFeast = "γιορτάζουν οι: Ευγενία, Ευγένιος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 359 Then
                ChurchFeast = "+Η ΚΑΤΑ ΣΑΡΚΑ ΓΕΝΝΗΣΙΣ ΤΟΥ ΚΥΡΙΟΥ ΚΑΙ ΣΩΤΗΡΟΣ " + vbCrLf + "ΗΜΩΝ ΙΗΣΟΥ ΧΡΙΣΤΟΥ, "
                NameFeast = "γιορτλάζουν οι: Χρήστος, Χρηστίνα, Χρύσα, Μανώλης," + vbCrLf + "Εμμανουέλλα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:39 Δύση Ήλιου 17:11"

            ElseIf DayOfYear = 360 Then
                ChurchFeast = "+Η σύναξις Της Θεοτόκου, Ευθυμίου, "
                NameFeast = "γιορτάζει ο Δαβίδ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:12"

            ElseIf DayOfYear = 361 Then
                ChurchFeast = "+Στεφάνου Πρωτομάρτ., Θεοδώρου, "
                NameFeast = "γιορτάζει ο Στέφανος."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 362 Then
                ChurchFeast = "Των εν Νικομηδεία Δισμυρίων Μαρτύρων, "
                NameFeast = "γιορτάζει η Δόμνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:40 Δύση Ήλιου 17:13"

            ElseIf DayOfYear = 363 Then
                ChurchFeast = "Των Υπό Ηρώδου Αναιρεθέντων Νηπίων, "
                NameFeast = ""
                WorldDay = "Παγκόσμια Ημέρα για τη Βιοποικιλότητα."
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:14"

            ElseIf DayOfYear = 364 Then
                '30 Δεκεμβρίου
                ChurchFeast = "+ΜΕΤΑ ΧΡΙΣΤΟΥ ΓΕΝΝ., Ανυσίας, Φιλεταίρ, "
                NameFeast = "γιορτάζουν οι: Ανύσιος, Ιωσήφ, Σήφης, Ιωσηφίνα, " + vbCrLf + "Ζοζεφίνα."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:15"

            ElseIf DayOfYear = 365 Then
                ChurchFeast = "Μελάνης οσίας, Ζωτικού Ορφανοτροφου, "
                NameFeast = "γιορτάζουν οι: Μελανία, Μέλανυ."
                WorldDay = ""
                SunRise = "Ανατολή Ήλιου 7:41 Δύση Ήλιου 17:16"
            End If
        End If
        Return ChurchFeast + vbCrLf + NameFeast + vbCrLf + WorldDay + vbCrLf + SunRise
    End Function
End Class
