object FrameThread: TFrameThread
  Left = 0
  Top = 0
  Width = 536
  Height = 250
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 250
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object memLog: TMemo
      Left = 0
      Top = 28
      Width = 536
      Height = 222
      Align = alClient
      TabOrder = 0
    end
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 536
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      Caption = 'pnlTop'
      TabOrder = 1
      object sbtUpdate: TSpeedButton
        Left = 0
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        AllowAllUp = True
        GroupIndex = 1
        Down = True
        Caption = 'Update'
        Flat = True
        OnClick = sbtUpdateClick
      end
      object btnStart: TButton
        Left = 50
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        Caption = 'Start'
        TabOrder = 0
        OnClick = btnStartClick
      end
      object btnStop: TButton
        Left = 100
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        Caption = 'Stop'
        TabOrder = 1
        OnClick = btnStopClick
      end
      object btnDelete: TButton
        Left = 150
        Top = 0
        Width = 50
        Height = 28
        Align = alLeft
        Caption = 'Delete'
        TabOrder = 2
        OnClick = btnDeleteClick
      end
      object ediStatus: TEdit
        Left = 200
        Top = 0
        Width = 336
        Height = 28
        Align = alClient
        TabOrder = 3
        ExplicitHeight = 21
      end
    end
  end
end
