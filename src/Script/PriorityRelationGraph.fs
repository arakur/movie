namespace Script

open FSharpPlus
open FSharpPlus.Data
open FSharpx.Collections
open System.Collections.Generic

type PriorityRelationGraphNode =
    { Succ: AssetRef Set
      Pred: AssetRef Set }

    static member empty = { Succ = Set.empty; Pred = Set.empty }

    static member succ this = this.Succ

    static member pred this = this.Pred

    static member addSucc node this =
        { this with
            Succ = this.Succ |> Set.add node }

    static member addPred node this =
        { this with
            Pred = this.Pred |> Set.add node }

    static member removeSucc node this =
        { this with
            Succ = this.Succ |> Set.remove node }

    static member removePred node this =
        { this with
            Pred = this.Pred |> Set.remove node }

type PriorityRelationGraph =
    { NextEdgeIndex: int
      Edges: Map<AssetRef, PriorityRelationGraphNode>
      EdgeIndices: Map<AssetRef * AssetRef, int> }

    static member empty =
        { NextEdgeIndex = 0
          Edges = Map.empty
          EdgeIndices = Map.empty }

    static member addNode node this =
        let edges' = this.Edges |> Map.add node PriorityRelationGraphNode.empty

        { this with Edges = edges' }

    static member addEdge lhs rhs this =
        // Memoized DFS to collect all edges, which is the minimal set of edges, which resolves the conflict in adding lhs -> rhs,
        // with choosing the minimum one respect to edge indices.

        // Create a cache to memoize the result of DFS.

        let mutable cache = Dictionary()

        cache.Add(lhs, (DList.empty, System.Int32.MaxValue))

        let cacheResult current ret =
            cache.[current] <- ret
            ret

        // DFS procedure.

        let rec search current =
            let nbh = this.Edges.[current].Succ

            let unionResults (edge0, minIndex0) (edge1, minIndex1) =
                DList.append edge0 edge1, min minIndex0 minIndex1

            let collectNbh () =
                // REMARK: Appending is in O(1) × #nbh, thus total time complexity for appending is O(E).
                nbh |> Seq.map (collectNext current) |> Seq.reduce unionResults

            cache.TryFind current |> Option.defaultWith (collectNbh >> cacheResult current)

        and collectNext current next =
            let currentEdge = current, next
            let currentIndex = this.EdgeIndices.[currentEdge]

            let children, childrenMinIndex = search next

            if currentIndex < childrenMinIndex then
                DList.singleton currentEdge, currentIndex
            else
                children, childrenMinIndex

        // Collect edges to remove.

        let edgesRemove, _ = search rhs

        // Remove edges and add an edge lhs -> rhs.

        let removeEdge edges (remove0, remove1) =
            edges
            |> Map.updateWith (PriorityRelationGraphNode.removeSucc remove1 >> Some) remove0
            |> Map.updateWith (PriorityRelationGraphNode.removePred remove0 >> Some) remove1

        let edges' =
            (this.Edges, edgesRemove)
            ||> Seq.fold removeEdge
            |> Map.updateWith (PriorityRelationGraphNode.addSucc rhs >> Some) lhs
            |> Map.updateWith (PriorityRelationGraphNode.addPred lhs >> Some) rhs

        let edgeIndices' = this.EdgeIndices |> Map.add (lhs, rhs) this.NextEdgeIndex

        let index' = this.NextEdgeIndex + 1

        { Edges = edges'
          EdgeIndices = edgeIndices'
          NextEdgeIndex = index' }