object Form1: TForm1
  Left = 393
  Top = 222
  Width = 502
  Height = 222
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvGroupHeader1: TJvGroupHeader
    Left = 8
    Top = 8
    Width = 225
    Height = 17
    Caption = 'JvGroupHeader1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object JvGroupHeader2: TJvGroupHeader
    Left = 8
    Top = 40
    Width = 225
    Height = 17
    Caption = 'JvGroupHeader2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    BevelOptions.Brush.Color = 11106843
    BevelOptions.Height = 3
    BevelOptions.Pen.Color = 14789952
    BevelOptions.Style = bsShape
  end
  object JvFooter1: TJvFooter
    Left = 0
    Top = 158
    Width = 494
    Height = 37
    Align = alBottom
    object JvFooterBtn1: TJvFooterBtn
      Left = 5
      Top = 9
      Width = 74
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Help'
      TabOrder = 0
      Alignment = taLeftJustify
      ButtonIndex = 0
      SpaceInterval = 6
    end
    object JvFooterBtn2: TJvFooterBtn
      Left = 334
      Top = 9
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      ButtonIndex = 1
      SpaceInterval = 6
    end
    object JvFooterBtn3: TJvFooterBtn
      Left = 415
      Top = 9
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancelar'
      TabOrder = 2
      ButtonIndex = 2
      SpaceInterval = 6
    end
  end
  object JvFooter2: TJvFooter
    Left = 0
    Top = 121
    Width = 494
    Height = 37
    Align = alBottom
    object JvFooterBtn4: TJvFooterBtn
      Left = 260
      Top = 9
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Anterior'
      TabOrder = 0
      ButtonIndex = 0
      SpaceInterval = 0
    end
    object JvFooterBtn5: TJvFooterBtn
      Left = 334
      Top = 9
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Seguinte'
      Default = True
      TabOrder = 1
      ButtonIndex = 1
      SpaceInterval = 6
    end
    object JvFooterBtn6: TJvFooterBtn
      Left = 415
      Top = 9
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Fechar'
      TabOrder = 2
      ButtonIndex = 2
      SpaceInterval = 6
    end
  end
  object JvFooter3: TJvFooter
    Left = 0
    Top = 84
    Width = 494
    Height = 37
    Align = alBottom
    object JvFooterBtn7: TJvFooterBtn
      Left = 210
      Top = 9
      Width = 74
      Height = 23
      Anchors = [akBottom]
      Caption = 'OK'
      TabOrder = 0
      Alignment = taCenter
      ButtonIndex = 0
      SpaceInterval = 0
    end
  end
end
