object Form1: TForm1
  Left = 131
  Top = 73
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clRed
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Animate1: TAnimate
    Left = 200
    Top = 128
    Width = 272
    Height = 60
    Active = True
    CommonAVI = aviCopyFile
    StopFrame = 26
  end
  object JvPanel1: TJvPanel
    Left = 464
    Top = 120
    Width = 257
    Height = 246
    Sizeable = True
    MultiLine = True
    Caption = 
      'JvPanel1 JvPanel1 JvPanel1 JvPanel1 JvPanel1 JvPanel1 JvPanel1 J' +
      'vPanel1 '
    TabOrder = 0
    object Label1: TLabel
      Left = 40
      Top = 128
      Width = 31
      Height = 13
      Caption = 'Label1'
      Transparent = True
    end
    object Edit1: TEdit
      Left = 24
      Top = 64
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object JvFilenameEdit1: TJvFilenameEdit
      Left = 24
      Top = 32
      Width = 121
      Height = 21
      AddQuotes = False
      ButtonFlat = False
      NumGlyphs = 1
      TabOrder = 1
      Text = 'JvFilenameEdit1'
    end
    object CheckBox2: TCheckBox
      Left = 30
      Top = 90
      Width = 97
      Height = 17
      Caption = 'Auto Arrange'
      TabOrder = 2
      OnClick = CheckBox2Click
    end
    object Panel1: TPanel
      Left = 20
      Top = 135
      Width = 66
      Height = 31
      TabOrder = 3
      object Label2: TLabel
        Left = 5
        Top = 10
        Width = 19
        Height = 13
        Caption = 'Left'
      end
      object LeftMaskEdit: TMaskEdit
        Left = 25
        Top = 5
        Width = 23
        Height = 21
        EditMask = '09;0;_'
        MaxLength = 2
        TabOrder = 0
        OnChange = LeftMaskEditChange
      end
    end
    object Panel2: TPanel
      Left = 20
      Top = 180
      Width = 66
      Height = 31
      TabOrder = 4
      object Label3: TLabel
        Left = 5
        Top = 10
        Width = 18
        Height = 13
        Caption = 'Top'
      end
      object TopMaskEdit: TMaskEdit
        Left = 25
        Top = 5
        Width = 25
        Height = 21
        EditMask = '09;0;_'
        MaxLength = 2
        TabOrder = 0
        Text = '0'
        OnChange = TopMaskEditChange
      end
    end
    object Panel3: TPanel
      Left = 135
      Top = 150
      Width = 96
      Height = 31
      TabOrder = 5
      object Label4: TLabel
        Left = 5
        Top = 10
        Width = 48
        Height = 13
        Caption = 'Horizontal'
      end
      object HorizontalMaskEdit: TMaskEdit
        Left = 55
        Top = 5
        Width = 27
        Height = 21
        EditMask = '09;0;_'
        MaxLength = 2
        TabOrder = 0
        Text = '5'
        OnChange = HorizontalMaskEditChange
      end
    end
    object Panel4: TPanel
      Left = 105
      Top = 190
      Width = 81
      Height = 31
      TabOrder = 6
      object Label5: TLabel
        Left = 5
        Top = 10
        Width = 35
        Height = 13
        Caption = 'Vertical'
      end
      object VerticalMaskEdit: TMaskEdit
        Left = 45
        Top = 5
        Width = 29
        Height = 21
        EditMask = '09;0;_'
        MaxLength = 2
        TabOrder = 0
        Text = '5'
        OnChange = VerticalMaskEditChange
      end
    end
  end
  object CheckBox1: TCheckBox
    Left = 344
    Top = 368
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object JvFormStorage1: TJvFormStorage
    StoredValues = <>
    Left = 416
    Top = 296
  end
end
