object JvBdeErrorDlg: TJvBdeErrorDlg
  Left = 202
  Top = 100
  ActiveControl = OKBtn
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 252
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BasicPanel: TPanel
    Left = 0
    Top = 0
    Width = 288
    Height = 108
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ErrorText: TLabel
      Left = 49
      Top = 10
      Width = 231
      Height = 88
      Align = alClient
      WordWrap = True
    end
    object IconPanel: TPanel
      Left = 0
      Top = 10
      Width = 49
      Height = 88
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object IconImage: TImage
        Left = 6
        Top = 1
        Width = 34
        Height = 34
      end
    end
    object TopPanel: TPanel
      Left = 0
      Top = 0
      Width = 288
      Height = 10
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
    end
    object RightPanel: TPanel
      Left = 280
      Top = 10
      Width = 8
      Height = 88
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
    end
    object BottomPanel: TPanel
      Left = 0
      Top = 98
      Width = 288
      Height = 10
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
    end
  end
  object DetailsPanel: TPanel
    Left = 0
    Top = 108
    Width = 380
    Height = 144
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 2
    object BDELabel: TJvLabel
      Left = 87
      Top = 11
      Width = 121
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object NativeLabel: TJvLabel
      Left = 87
      Top = 30
      Width = 121
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
    end
    object DbMessageText: TMemo
      Left = 7
      Top = 53
      Width = 366
      Height = 54
      TabStop = False
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      WantReturns = False
    end
    object DbResult: TEdit
      Left = 214
      Top = 8
      Width = 80
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
    object DbCatSub: TEdit
      Left = 293
      Top = 8
      Width = 80
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
    object NativeResult: TEdit
      Left = 214
      Top = 27
      Width = 159
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 3
    end
    object Back: TButton
      Left = 210
      Top = 112
      Width = 79
      Height = 25
      TabOrder = 4
      OnClick = BackClick
    end
    object Next: TButton
      Left = 294
      Top = 112
      Width = 79
      Height = 25
      TabOrder = 5
      OnClick = NextClick
    end
  end
  object ButtonPanel: TPanel
    Left = 288
    Top = 0
    Width = 92
    Height = 108
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object DetailsBtn: TButton
      Left = 7
      Top = 65
      Width = 79
      Height = 25
      TabOrder = 1
      OnClick = DetailsBtnClick
    end
    object OKBtn: TButton
      Left = 7
      Top = 12
      Width = 79
      Height = 25
      Cancel = True
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
