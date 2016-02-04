//
//  Lens.swift
//  Created by Safx Developer on 2016/02/02.
//
//  The following source is originally from Lenso available at:
//   https://github.com/narfdotpl/lenso
//

//
// The MIT License (MIT)
//
// Copyright (c) 2016 Maciej Konieczny
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.


// Lenses API

protocol LensType {
    typealias Whole
    typealias Part
    var get: Whole -> Part { get }
    var set: (Part, Whole) -> Whole { get }
}

struct Lens<Whole, Part>: LensType {
    let get: Whole -> Part
    let set: (Part, Whole) -> Whole
}

struct ArrayLens<Whole, Element>: LensType {
    typealias Part = [Element]
    let get: Whole -> Part
    let set: (Part, Whole) -> Whole
}

extension LensType {
    func compose<Subpart, L: LensType where Self.Part == L.Whole, L.Part == Subpart>(other: L) -> Lens<Whole, Subpart> {
        return Lens<Whole, Subpart>(
            get: { whole in
                return other.get(self.get(whole))
            },
            set: { newSubpart, whole in
                return self.set(other.set(newSubpart, self.get(whole)), whole)
            }
        )
    }
    func modify(closure: Part -> Part, from: Whole) -> Whole {
        return set(closure(get(from)), from)
    }
}

extension ArrayLens {
    func at(idx: Int) -> Lens<Whole, Element> {
        return Lens<Whole, Element>(
            get: { whole in
                return self.get(whole)[idx]
            },
            set: { newSubpart, whole in
                var xs = self.get(whole)
                xs[idx] = newSubpart
                return self.set(xs, whole)
            }
        )
    }
}

// lenses API Helper

func createIdentityLens<Whole>() -> Lens<Whole, Whole> {
    return Lens<Whole, Whole>(
        get: { $0 },
        set: { (new, old) in return new }
    )
}

// Bound lenses API

struct BoundLensStorage<Whole, Part> {
    let instance: Whole
    let lens: Lens<Whole, Part>
}


protocol BoundLensType {
    typealias Whole
    typealias Part

    init(storage: BoundLensStorage<Whole, Part>)

    var storage: BoundLensStorage<Whole, Part> { get }

    func get() -> Part
    func set(newPart: Part) -> Whole
}

extension BoundLensType {
    init(instance: Whole, lens: Lens<Whole, Part>) {
        self.init(storage: BoundLensStorage(instance: instance, lens: lens))
    }

    init<Parent: BoundLensType, L: LensType where Parent.Whole == Whole, Self.Part == L.Part, Parent.Part == L.Whole>(parent: Parent, sublens: L) {
        let s = parent.storage
        self.init(storage: BoundLensStorage(instance: s.instance, lens: s.lens.compose(sublens)))
    }

    func get() -> Part {
        return storage.lens.get(storage.instance)
    }

    func set(newPart: Part) -> Whole {
        return storage.lens.set(newPart, storage.instance)
    }
}


struct BoundLens<Whole, Part>: BoundLensType {
    let storage: BoundLensStorage<Whole, Part>
}
