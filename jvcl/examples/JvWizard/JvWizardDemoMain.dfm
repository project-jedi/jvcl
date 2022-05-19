object Form1: TForm1
  Left = 0
  Top = 0
  ActiveControl = JvWizardWelcomePage
  Caption = 'Form1'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvWizard1: TJvWizard
    Left = 0
    Top = 0
    Width = 852
    Height = 411
    ActivePage = JvWizardWelcomePage
    ButtonBarHeight = 42
    ButtonStart.Caption = 'To &Start Page'
    ButtonStart.NumGlyphs = 1
    ButtonStart.Width = 85
    ButtonLast.Caption = 'To &Last Page'
    ButtonLast.NumGlyphs = 1
    ButtonLast.Width = 85
    ButtonBack.Caption = '< &Back'
    ButtonBack.NumGlyphs = 1
    ButtonBack.Width = 75
    ButtonNext.Caption = '&Next >'
    ButtonNext.NumGlyphs = 1
    ButtonNext.Width = 75
    ButtonFinish.Caption = '&Finish'
    ButtonFinish.NumGlyphs = 1
    ButtonFinish.Width = 75
    ButtonCancel.Caption = 'Cancel'
    ButtonCancel.NumGlyphs = 1
    ButtonCancel.ModalResult = 2
    ButtonCancel.Width = 75
    ButtonHelp.Caption = '&Help'
    ButtonHelp.NumGlyphs = 1
    ButtonHelp.Width = 75
    ShowRouteMap = True
    ExplicitLeft = 432
    ExplicitTop = 224
    ExplicitWidth = 100
    ExplicitHeight = 40
    DesignSize = (
      852
      411)
    object JvWizardWelcomePage: TJvWizardWelcomePage
      Header.Title.Color = clNone
      Header.Title.Text = 'Welcome'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'JvWizard Example'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      VisibleButtons = [bkNext, bkCancel]
      Caption = 'JvWizardWelcomePage'
      ExplicitLeft = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object JvWizardInteriorPage1: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Page 1'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Enter something in the textbox to continue'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      Caption = 'JvWizardInteriorPage1'
      OnNextButtonClick = JvWizardInteriorPage1NextButtonClick
      object Label1: TLabel
        Left = 16
        Top = 152
        Width = 78
        Height = 13
        Caption = 'Enter something'
      end
      object EnterEdit: TEdit
        Left = 16
        Top = 168
        Width = 321
        Height = 21
        TabOrder = 0
      end
    end
    object JvWizardInteriorPage2: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Page 2'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'Change the checkboxes'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      Caption = 'JvWizardInteriorPage2'
      OnEnterPage = JvWizardInteriorPage2EnterPage
      ExplicitLeft = 151
      object ContinueCheckBox: TCheckBox
        Left = 16
        Top = 120
        Width = 177
        Height = 17
        Caption = 'Activate to continue'
        TabOrder = 0
        OnClick = ContinueCheckBoxClick
      end
      object SkipCheckBox: TCheckBox
        Left = 16
        Top = 152
        Width = 177
        Height = 17
        Caption = 'Skip next page'
        TabOrder = 1
        OnClick = ContinueCheckBoxClick
      end
    end
    object JvWizardInteriorPage3: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Skippable Page'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Text = 'This page can be skipped on the Page before'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      Caption = 'JvWizardInteriorPage3'
    end
    object JvWizardFinalPage: TJvWizardInteriorPage
      Header.Title.Color = clNone
      Header.Title.Text = 'Final Page'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'Tahoma'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'Tahoma'
      Header.Subtitle.Font.Style = []
      Header.Subtitle.Text = ''
      VisibleButtons = [bkBack, bkFinish]
      Caption = 'JvWizardFinalPage'
      OnFinishButtonClick = JvWizardFinalPageFinishButtonClick
      ExplicitLeft = 151
    end
    object JvWizardRouteMapList1: TJvWizardRouteMapList
      Left = 0
      Top = 0
      Width = 145
      Height = 369
      Cursor = crHandPoint
      ActiveFont.Charset = DEFAULT_CHARSET
      ActiveFont.Color = clWindowText
      ActiveFont.Height = -11
      ActiveFont.Name = 'Tahoma'
      ActiveFont.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clNavy
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsUnderline]
      ItemText = itTitle
      ExplicitLeft = 352
      ExplicitTop = 208
      ExplicitHeight = 0
    end
  end
end
