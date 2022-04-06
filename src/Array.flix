namespace Array {
    pub def toMList(a: ScopedArray[a, r]): MList[a, r] \ { Read(r), Write(r) } =
        let m = new MList(Scoped.regionOf(a));
        def loop(i) = if (i < a.length) MList.push!(a[i], m) else ();
        loop(0);
        m
}
