using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;

namespace AndroidJNIConstructor
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(AndroidJNIConstructorCodeFixProvider)), Shared]
    public class AndroidJNIConstructorCodeFixProvider : CodeFixProvider
    {
        private const string title = "Add protected JNI constructor";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(AndroidJNIConstructorAnalyzer.DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest
            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the type declaration identified by the diagnostic.
            var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: title,
                    createChangedDocument: c => IncludeJniConstructor(context.Document, declaration, c),
                    equivalenceKey: title),
                diagnostic);
        }

        private async Task<Document> IncludeJniConstructor(Document document, TypeDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {
            (bool systemUsing, bool androidRuntimeUsing) = AreUsingAvailables(classDeclaration);

            ConstructorDeclarationSyntax jniConstructor = CreateJniConstructor(classDeclaration.Identifier, systemUsing, androidRuntimeUsing);
            List<MemberDeclarationSyntax> members = classDeclaration.Members.ToList();
            if (FindOtherConstructor(classDeclaration, out int defaultConstructorIndex))
            {
                members.Insert(defaultConstructorIndex + 1, jniConstructor);
            }
            else
            {
                members.Insert(0, jniConstructor);
                members.Insert(0, CreateEmptyDefaultConstructor(classDeclaration.Identifier));
            }

            TypeDeclarationSyntax classWithCorrectConstructor = classDeclaration.WithMembers(SyntaxFactory.List<MemberDeclarationSyntax>(members));

            var oldRoot = await document.GetSyntaxRootAsync(cancellationToken);
            var newRoot = oldRoot.ReplaceNode(classDeclaration, classWithCorrectConstructor);

            return document.WithSyntaxRoot(newRoot);
        }

        private bool FindOtherConstructor(TypeDeclarationSyntax classDeclaration, out int index)
        {
            for (int i = 0; i < classDeclaration.Members.Count; ++i)
            {
                if (classDeclaration.Members[i].IsKind(SyntaxKind.ConstructorDeclaration))
                {
                    index = i;
                    return true;
                }
            }

            index = -1;
            return false;
        }

        private ConstructorDeclarationSyntax CreateJniConstructor(SyntaxToken classIdentifier, bool systemUsing, bool androidRuntimeUsing)
        {
            return SyntaxFactory.ConstructorDeclaration(classIdentifier)
                .WithModifiers(SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.ProtectedKeyword)))
                .WithParameterList(SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList<ParameterSyntax>(new ParameterSyntax[]{
                        SyntaxFactory.Parameter(SyntaxFactory.Identifier("javaReference"))
                            .WithType(SyntaxFactory.IdentifierName(TypeName("System", "IntPtr", systemUsing))),
                        SyntaxFactory.Parameter(SyntaxFactory.Identifier("handle"))
                            .WithType(SyntaxFactory.IdentifierName(TypeName("Android.Runtime", "JniHandleOwnership", androidRuntimeUsing)))
                    })))
                .WithBody(SyntaxFactory.Block())
                .WithInitializer(SyntaxFactory.ConstructorInitializer(SyntaxKind.BaseConstructorInitializer)
                .AddArgumentListArguments(
                        SyntaxFactory.Argument(SyntaxFactory.IdentifierName("javaReference")),
                        SyntaxFactory.Argument(SyntaxFactory.IdentifierName("handle"))
                    ));

            string TypeName(string namespaceName, string typeName, bool usingAvailable)
            {
                if(usingAvailable)
                {
                    return typeName;
                }
                return $"{namespaceName}.{typeName}";
            }
        }

        private ConstructorDeclarationSyntax CreateEmptyDefaultConstructor(SyntaxToken classIdentifier)
        {
            return SyntaxFactory.ConstructorDeclaration(classIdentifier)
                .WithModifiers(SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                .WithBody(SyntaxFactory.Block());
        }

        private List<UsingDirectiveSyntax> AggregateUsingsInParents(SyntaxNode node)
        {
            List<UsingDirectiveSyntax> usings = new List<UsingDirectiveSyntax>(10);
            while (node != null)
            {
                if (node is CompilationUnitSyntax unitSyntax)
                {
                    usings.AddRange(unitSyntax.Usings);
                }
                else if (node is NamespaceDeclarationSyntax namespaceSyntax)
                {
                    usings.AddRange(namespaceSyntax.Usings);
                }
                node = node.Parent;
            }

            return usings;
        }

        private (bool systemUsing, bool androidRuntimeUsing) AreUsingAvailables(SyntaxNode node)
        {
            var usings = AggregateUsingsInParents(node);

            bool systemUsing = false;
            bool androidRuntimeUsing = false;

            foreach(var usingSyntax in usings)
            {
                var name = usingSyntax.Name;

                if(usingSyntax.Name is IdentifierNameSyntax nameSyntax)
                {
                    string namespaceName = nameSyntax.Identifier.ValueText;

                    if(namespaceName == "System")
                    {
                        systemUsing = true;
                    }
                }

                if(usingSyntax.Name is QualifiedNameSyntax qualifiedNameSyntax)
                {
                    string namespaceName = qualifiedNameSyntax.ToString();

                    if (namespaceName == "Android.Runtime")
                    {
                        androidRuntimeUsing = true;
                    }
                }
            }

            return (systemUsing, androidRuntimeUsing);
        }
    }
}
