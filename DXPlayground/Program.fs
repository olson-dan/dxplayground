
#light "off"

module Program
 
open System.IO
open SlimDX
open SlimDX.DXGI
open SlimDX.Direct3D11
open SlimDX.D3DCompiler
open SlimDX.Windows

open Video

 
let emitCSCode = @"
	struct State
	{
		float4 position_size_time; // xy = pos, z = size, w = time
		float4 lifetime_seed; // x = lifetime, y = seed, z = _, w = _
	};
	RWStructuredBuffer<State> state;
	RWBuffer<uint> dispatch;
 
	cbuffer globals { float frameTime; }

	float rval (float start, float end, inout uint rng_state)
	{
		rng_state = 1664525 * rng_state + 1013904223;
		float f0 = float(rng_state) * (1.0 / 4294967296.0);
		return f0 * (end - start) + start;
	}
 
	[numthreads(1,1,1)]
	void cs_main (uint3 threadID : SV_DispatchThreadID)
	{
		uint numParticles = dispatch[0];
 
		uint rng_state = dispatch[3];
 
		float posX = rval (0.0, 0.001, rng_state);
		float posY = rval (0.0, 0.001, rng_state);
 
		float lifeTime = rval (1, 100, rng_state);
 
		State s;
		s.position_size_time = float4 (posX, posY, 0, 0);
		s.lifetime_seed = float4 (lifeTime, asfloat(rng_state), 0, 0);
 
		state[numParticles] = s;
		dispatch[0] = numParticles + 1;
		dispatch[1] = 1;
		dispatch[2] = 1;
		dispatch[3] = rng_state;
	}"
 
(*struct DrawInstancedIndirectArgs {
  UINT VertexCountPerInstance;
  UINT InstanceCount;
  UINT StartVertexLocation;
  UINT StartInstanceLocation;
}*)
 
let updateCSCode = @"
	struct State
	{
		float4 position_size_time; // xy = pos, z = size, w = time
		float4 lifetime_seed; // x = lifetime, y = seed, z = _, w = _
	};
	StructuredBuffer<State> inputState;
	AppendStructuredBuffer<State> outputState;
	RWStructuredBuffer<float4> verts;
 
	//cbuffer globals { float frameTime; }
 
	float rval (float start, float end, inout uint rng_state)
	{
		rng_state = 1664525 * rng_state + 1013904223;
		float f0 = float(rng_state) * (1.0 / 4294967296.0);
		return f0 * (end - start) + start;
	}
	
	[numthreads(1,1,1)]
	void cs_main (uint3 threadID : SV_DispatchThreadID)
	{
		State particle = inputState[threadID.x];
		uint rng_state = asuint(particle.lifetime_seed.y);
 
		float progress = particle.position_size_time.w / particle.lifetime_seed.x;
 
		if (progress <= 1.0f)
		{
			float step = 0.016;
			float time = particle.position_size_time.w + step;
 
			float velXStart = rval (-10 / 1280.0, 10 / 1280.0, rng_state);
			float velXEnd = rval (-10 / 1280.0, 10 / 1280.0, rng_state);
			float velYStart = rval (-10 / 720.0, 10 / 720.0, rng_state);
			float velYEnd = rval (-10 / 720.0, 10 / 720.0, rng_state);
			float sizeStart = rval (1, 10, rng_state);
			float sizeEnd = rval (1, 10, rng_state);
 
			float velX = lerp (velXStart, velXEnd, progress);
			float velY = lerp (velYStart, velYEnd, progress);
 
			float size = lerp (sizeStart, sizeEnd, progress);
 
			float2 pos = particle.position_size_time.xy + float2 (velX, velY) * step;
 
			particle.position_size_time = float4 (pos, size, time);
 
			outputState.Append (particle);
			verts[verts.IncrementCounter()] = float4 (pos, size / 1280.0, size / 720.0);
		}
	}"
 
let updateBuffersCSCode = @"
	RWStructuredBuffer<float4> verts;
	RWBuffer<uint> dispatch;
	RWBuffer<uint> draw;
 
	[numthreads(1,1,1)]
	void cs_main (uint3 threadID : SV_DispatchThreadID)
	{
		uint count = verts.IncrementCounter();
 
		dispatch[0] = count;
		draw[0] = count;
		draw[1] = 1;
	}"
 
let pointSpriteVSCode = @"
	StructuredBuffer<float4> verts;
 
	float4 vs_main (uint vertex : SV_VertexID) : SV_POSITION
	{
		float4 v = verts[vertex / 6];
		float2 pos = v.xy;
		float2 pointSize = v.zw;
		switch (vertex % 6)
		{
			case 0:  pos += pointSize * float2(-1,1); break;
			case 1:  case 4: pos += pointSize * float2 (1,1); break;
			case 2:  case 3: pos += pointSize * float2 (-1,-1); break;
			case 5: pos += pointSize * float2 (1,-1); break;
		}
		return float4 (pos, 0, 1);
	}"
 
let pointSpritePSCode = @"
	float4 ps_main (float4 vertex : SV_POSITION) : SV_Target
	{
		return float4 (1,1,0,1);
	}"
 
let bufferFromSeq device stride data =
	let size = 4 * (Seq.length data) in
	let stream = new DataStream(int64 size, true, true) in
	for x in data do stream.Write (float32 x) done;
	stream.Position <- 0L;
	new ShaderResourceView (device, new Buffer (device, stream, size, ResourceUsage.Default,
		BindFlags.ShaderResource, CpuAccessFlags.None, ResourceOptionFlags.StructuredBuffer, stride))
 
let rwBufferOfSize device items stride =
	let size = items * stride in
	new Buffer (device, size,
		ResourceUsage.Default,
		(BindFlags.ShaderResource ||| BindFlags.UnorderedAccess),
		CpuAccessFlags.None,
		ResourceOptionFlags.StructuredBuffer,
		stride)
 
let indirectBufferOfSize device items stride =
	let size = items * stride in
	let stream = new DataStream(int64 size, true, true) in
	for x in 1..(size/4) do stream.Write(0.0f) done;
	stream.Position <- 0L;
	new Buffer (device, stream, size,
		ResourceUsage.Default,
		(BindFlags.ShaderResource ||| BindFlags.UnorderedAccess),
		CpuAccessFlags.None,
		ResourceOptionFlags.DrawIndirect,
		stride)
 
let indirectBufferOfSizeWithSeed device items stride (rng:int) =
	let size = items * stride in
	let stream = new DataStream(int64 size, true, true) in
	for x in 1..(stride/4) do stream.Write(0.0f) done;
	stream.WriteRange<byte>(System.BitConverter.GetBytes(rng));
	stream.Position <- 0L;
	new Buffer (device, stream, size,
		ResourceUsage.Default,
		(BindFlags.ShaderResource ||| BindFlags.UnorderedAccess),
		CpuAccessFlags.None,
		ResourceOptionFlags.DrawIndirect,
		stride)
 
[<EntryPoint>]
let main argv =
	let w, h = 1280, 720 in
	let window = Video.Window("Test Window", w, h) in
	let context = window.Context in
	let device = window.Device in
 
	let emitCS = emitCSCode |> Video.CompileCS device in
	let updateCS = updateCSCode |> Video.CompileCS device in
	let updateBuffersCS = updateBuffersCSCode |> Video.CompileCS device in
	let pointSpriteVS = pointSpriteVSCode |> Video.CompileVS device in
	let pointSpritePS = pointSpritePSCode |> Video.CompilePS device in
 
	let maxParticles, numRands = 100000, 5000 in
 
	let state1Buffer = rwBufferOfSize device maxParticles 32 in
	let state1SRV = new ShaderResourceView (device, state1Buffer) in
	let state1Desc = new UnorderedAccessViewDescription(
		Dimension = UnorderedAccessViewDimension.Buffer,
		Flags = UnorderedAccessViewBufferFlags.AllowAppend,
		Format = Format.Unknown,
		ElementCount = maxParticles
	) in
	let state1AppendUAV = new UnorderedAccessView (device, state1Buffer, state1Desc) in
	let state1UAV = new UnorderedAccessView (device, state1Buffer) in
 
	let state2Buffer = rwBufferOfSize device maxParticles 32 in
	let state2SRV = new ShaderResourceView (device, state2Buffer) in
	let state2Desc = new UnorderedAccessViewDescription(
		Dimension = UnorderedAccessViewDimension.Buffer,
		Flags = UnorderedAccessViewBufferFlags.AllowAppend,
		Format = Format.Unknown,
		ElementCount = maxParticles
	) in
	let state2AppendUAV = new UnorderedAccessView (device, state2Buffer, state2Desc) in
	let state2UAV = new UnorderedAccessView (device, state2Buffer) in
 
	let dispatchBuffer = indirectBufferOfSizeWithSeed device 2 12 (System.Guid.NewGuid().GetHashCode()) in
	let dispatchDesc = new UnorderedAccessViewDescription(
		Dimension = UnorderedAccessViewDimension.Buffer,
		ElementCount = 6,
		Format = Format.R32_UInt) in
	let dispatchUAV = new UnorderedAccessView (device, dispatchBuffer, dispatchDesc) in
 
	let drawBuffer = indirectBufferOfSize device 1 16 in
	let drawDesc = new UnorderedAccessViewDescription(
		Dimension = UnorderedAccessViewDimension.Buffer,
		ElementCount = 4,
		Format = Format.R32_UInt) in
	let drawUAV = new UnorderedAccessView (device, drawBuffer, drawDesc) in
 
 
	let vertsBuffer = rwBufferOfSize device maxParticles 16 in
	let vertsSRV = new ShaderResourceView(device, vertsBuffer) in
	let vertsDesc = new UnorderedAccessViewDescription(
		Dimension = UnorderedAccessViewDimension.Buffer,
		Flags = UnorderedAccessViewBufferFlags.Counter,
		Format = Format.Unknown,
		ElementCount = maxParticles
	) in
	let vertsUAV = new UnorderedAccessView (device, vertsBuffer, vertsDesc) in
 
	let frame = ref 0 in
 
	window.Loop (fun () ->
		context.ClearRenderTargetView(window.RenderTarget, Color4());
 
		context.ComputeShader.SetUnorderedAccessView((if !frame % 2 = 0 then state1UAV else state2UAV), 0);
		context.ComputeShader.SetUnorderedAccessView(dispatchUAV, 1);
		context.ComputeShader.Set(emitCS);
 
		let numEmitters = 1 in
		context.Dispatch(numEmitters, 1, 1);
 
		context.ComputeShader.SetUnorderedAccessView(null, 1);
		context.ComputeShader.SetUnorderedAccessView(null, 0);
		context.ComputeShader.SetShaderResource(null, 1);
		context.ComputeShader.SetShaderResource(null, 0);
 
		context.ComputeShader.SetShaderResource((if !frame % 2 = 0 then state1SRV else state2SRV), 0);
		context.ComputeShader.SetUnorderedAccessView((if !frame % 2 = 0 then state2AppendUAV else state1AppendUAV), 0, 0);
		context.ComputeShader.SetUnorderedAccessView(vertsUAV, 1, 0);
		context.ComputeShader.Set(updateCS);
 
		context.DispatchIndirect(dispatchBuffer, 0);
 
		context.ComputeShader.SetUnorderedAccessView(null, 1);
		context.ComputeShader.SetUnorderedAccessView(null, 0);
		context.ComputeShader.SetShaderResource(null, 1);
		context.ComputeShader.SetShaderResource(null, 0);
 
 
		context.ComputeShader.SetUnorderedAccessView(vertsUAV, 0);
		context.ComputeShader.SetUnorderedAccessView(dispatchUAV, 1);
		context.ComputeShader.SetUnorderedAccessView(drawUAV, 2);
		context.ComputeShader.Set(updateBuffersCS);
 
		context.Dispatch(1,1,1);
		context.ComputeShader.SetUnorderedAccessView(null, 2);
		context.ComputeShader.SetUnorderedAccessView(null, 1);
		context.ComputeShader.SetUnorderedAccessView(null, 0);
 
 
		context.InputAssembler.PrimitiveTopology <- PrimitiveTopology.TriangleList;
 
		context.VertexShader.SetShaderResource(vertsSRV, 0);
		context.VertexShader.Set(pointSpriteVS);
		context.PixelShader.Set(pointSpritePS);
 
		context.DrawInstancedIndirect(drawBuffer, 0);
 
		context.VertexShader.SetShaderResource(null, 0);
 
		frame := 1 - !frame;
		window.Present
	);
	for item in ObjectTable.Objects do item.Dispose() done;
	0