// Copyright (C) 2023 Pontus Wiberg <wiberg.public@outlook.com>
// Licensed under GPLv2 or later version. Contact for any licensing concerns

unit uStart;

interface

procedure Start;
procedure Stop;
procedure Tick;

implementation

uses
  uTypes,
  uTask,

  Classes,
  Generics.Collections,
  SysUtils;

var
  FFac: IFac;
  FMan: ITaskCluster;

function RegularFunc: string;
begin
  Result := 'Regular func';
end;

procedure Test6;
var
  LProc: TProcArg1<ITask<Integer>>;
  LTaskOnce1, LTaskOnce2, LTaskCount1, LTaskCount2: ITask<Integer>;
begin
  LProc := procedure(const ATask: ITask<Integer>)
    begin
      ATask.Log('A log');
    end;

  LTaskOnce1 := TTask<Integer>.Create(tcFunc, 'FuncLogOnce', LProc);
  LTaskOnce2 := TTask<Integer>.Create(tcThread, 'ThreadLogOnce', LProc);

  FMan.Add(LTaskOnce1);
  FMan.Add(LTaskOnce2);

  LProc := procedure(const ATask: ITask<Integer>)
    begin
      while ATask.Ticking do
      begin
        ATask.SetData(ATask.Data + 1);
        ATask.Log(ATask.Data.ToString);
        ATask.ThreadSleep(500);
      end;
    end;

  LTaskCount1 := TTask<Integer>.Create(tcFunc, 'Func counter', LProc);
  LTaskCount2 := TTask<Integer>.Create(tcThread, 'Thread counter', LProc);

  FMan.Add(LTaskCount1);
  FMan.Add(LTaskCount2);

  FMan.Run;
end;

procedure Test7;
var
  LProc: TProcArg1<ITask<ILockWrap<TStringList>>>;
  LTask, LTask2: ITask<ILockWrap<TStringList>>;
  LData: ILockWrap<TStringList>;
begin
  LProc := procedure(const ATask: ITask < ILockWrap < TStringList >> )
    var
      LLock: ILock;
      LList: TStringList;
    begin
      LLock := ATask.Data;
      LList := ATask.Data.Ref;

      while ATask.Ticking do
      begin
        if not LLock.WriterGet then
          Continue;
        try
          LList.Values[ATask.GetName] := (LList.Values[ATask.GetName].ToInteger + 1).ToString;
          ATask.Log(LList.DelimitedText);
        finally
          LLock.WriterRel;
        end;
        ATask.ThreadSleep(100);
      end;
    end;

  // LData := FGet.LockWrap<TStringList>;
  LData := FGet.LockWrap<TStringList>;
  LData.SetData;
  // LData.SetData(TStringList.Create);

  LTask := TTask < ILockWrap < TStringList >>.Create(tcFunc, 'Func', LProc, LData);
  LTask2 := TTask < ILockWrap < TStringList >>.Create(tcThread, 'Thre', LProc, LData);

  LData.Ref.AddPair(LTask.GetName, '0');
  LData.Ref.AddPair(LTask2.GetName, '0');

  LTask.SetData(LData);
  LTask2.SetData(LData);

  FMan.Add(LTask);
  FMan.Add(LTask2);

  FMan.Run;
end;

procedure Start;
begin
  FFac := FGet.Fac;
  FMan := FFac.TaskClusterMain;
  // Test4;
  // Test5;
  Test6;
  Test7;
end;

procedure Stop;
begin
  FMan.Terminate;
  Tick;
end;

procedure Tick;
begin
  FMan.Refresh;
end;

end.
